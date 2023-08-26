{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main where

import           Plutus.Model         (Run,
                                       adaValue, defaultBabbage, mustFail, testNoErrors,
                                       toV2, Tx, payToKey, Ada (Lovelace), ada, newUser, submitTx, valueAt, logError, mintValue, TypedPolicy (TypedPolicy), payToScript, userSpend, UserSpend, TypedValidator (TypedValidator), DatumMode (InlineDatum), spend, currentTime, spendScript, currentTimeRad, validateIn, utxoAt, waitUntil)
import           PlutusTx.Prelude     (($), Integer, AdditiveGroup (..))
import           Prelude              (IO, (.), (<>), mconcat, Eq ((==)), (&&), Num ((+)))
import           ScholarshipPool
import           VerifiedByToken
import           MilestoneToken
import           Test.Tasty           (defaultMain, testGroup)
import Plutus.V1.Ledger.Api (PubKeyHash (getPubKeyHash), Value, singleton, TokenName (TokenName), POSIXTime (POSIXTime, getPOSIXTime), TxOutRef)
import Control.Monad (replicateM, mapM, unless)
import qualified Scholarship
import Scholarship (Scholarship(..))
import Plutus.V2.Ledger.Api (TxOut(txOutValue))

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Tests for ScholarshipPool Script"
      [ good "Create scholarship before deadline" (initializeScholarshipParamed initializeScholarshipTx 10000) -- Can create a scholarship in the intended manner before the deadline
      , bad  "Create scholarship after deadline" (initializeScholarshipParamed initializeScholarshipTx (-10000)) -- Cannot create a scholarship after the deadline
      , bad  "Send excess money back without datum" (initializeScholarshipParamed initializeScholarshipNoRefundDatumTx 10000) -- Cannot return the excess money back to the pooling script with no datum
      , bad  "Don't send all excess back" (initializeScholarshipParamed initializeScholarshipStealSomeExcessTx 10000) -- Cannot only send a portion of the excess money back to the pooling script
      , bad  "Send schol funds to self" (initializeScholarshipParamed initializeScholarshipSendScholToSelfTx 10000) -- Cannot burn tokens as if creating a scholarship, but then send the scholarship money directly to self
      , bad  "Send half schol funds to self" (initializeScholarshipParamed initializeScholarshipSendHalfScholToSelfTx 10000) -- Cannot create a scholarship with less than the intended amount in order to send money to self
      , bad  "Create Scholarship without tokens" (initializeScholarshipParamed initializeScholarshipNoTokensTx 10000) -- Cannot create scholarship without burning tokens
      , bad  "Create Scholarship with self-minted tokens" (initializeScholarshipSelfMintedToken 10000) -- Student cannot mint their own tokens from the VerifiedByToken script and use those to create a scholarship
      , bad  "Create Scholarship with wrong tokenName" (initializeScholarshipWrongTokenName 10000) -- If student accidentally sends their tokens to someone else, that other party cannot use those tokens to create a scholarship for themselves.
      , good "Authority refund after deadline" (authorityRefundDeadline (-10000)) -- Authority can refund after the deadline
      , bad  "Authority refund before deadline" (authorityRefundDeadline 10000) -- Authority cannot refund before the deadline
      , bad  "Non-authority refunds after deadline" (nonAuthorityRefundDeadline (-10000)) -- Non-authority cannot refund (after the deadline)
      ]
 where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTIONS/INSTANCES/TYPES ------------------------------------

-- Set many users at once
setup5Users :: Run [PubKeyHash]
setup5Users = replicateM 5 $ newUser (ada (Lovelace 1000_000_000))

valueVBT :: PubKeyHash -> PubKeyHash -> Integer -> Value
valueVBT pkhSender pkhReciever = singleton (VerifiedByToken.curSymbol pkhSender) (TokenName $ getPubKeyHash pkhReciever)

typedPolicyVBT :: PubKeyHash -> TypedPolicy ()
typedPolicyVBT pkhMinter = TypedPolicy (toV2 $ VerifiedByToken.policy pkhMinter)

scholarshipPoolScript :: Scholarship.Scholarship -> TypedValidator datum redeemer
scholarshipPoolScript scholarship = TypedValidator $ toV2 (ScholarshipPool.poolValidator scholarship (Scholarship.scholarshipValidatorHash scholarship) )

scholarshipScript :: Scholarship.Scholarship -> TypedValidator datum redeemer
scholarshipScript scholarship = TypedValidator $ toV2 (Scholarship.scholarshipValidator scholarship)

defineScholarship :: PubKeyHash -> PubKeyHash -> PubKeyHash -> Integer -> Integer -> POSIXTime -> Scholarship.Scholarship
defineScholarship auth school courseProvider amount milestones deadline =
  Scholarship.Scholarship {
        sAuthority      = auth
      , sAuthoritySym   = VerifiedByToken.curSymbol auth
      , sSchool         = school
      , sSchoolSym      = VerifiedByToken.curSymbol school
      , sCourseProvider = courseProvider
      , sCourseProviderSym = MilestoneToken.curSymbol courseProvider
      , sAmount         = amount
      , sMilestones     = milestones
      , sDeadline       = deadline
      }

-- Donate 100 Ada Twice
donate :: Scholarship.Scholarship -> UserSpend -> Tx
donate scholarship usp =
  mconcat
  [ userSpend usp
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (ada (Lovelace 100_000_000))
  ]

mintSendToken :: TypedPolicy () -> Value -> PubKeyHash -> Tx
mintSendToken typedPolicy valueMint pkhRecipient =
  mconcat
    [ mintValue typedPolicy () valueMint
    , payToKey pkhRecipient valueMint
    ]

-- Send Value to Pkh
sendTo :: UserSpend -> Value -> PubKeyHash -> Tx
sendTo usp value pkh=
  mconcat
  [ userSpend usp
  , payToKey pkh value
  ]

initializeScholarshipTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipTx student usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , mintValue (typedPolicyVBT (sSchool scholarship)) () (valueVBT (sSchool scholarship) student (-1))
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student 0)) (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 100_000_000))
  ]

initializeScholarshipNoRefundDatumTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipNoRefundDatumTx student usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , mintValue (typedPolicyVBT (sSchool scholarship)) () (valueVBT (sSchool scholarship) student (-1))
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student 0)) (ada (Lovelace 100_000_000))
  , payToKey (scholarshipPoolScript scholarship) (valueIn - ada (Lovelace 100_000_000))
  ]

initializeScholarshipStealSomeExcessTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipStealSomeExcessTx student usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , mintValue (typedPolicyVBT (sSchool scholarship)) () (valueVBT (sSchool scholarship) student (-1))
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student 0)) (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 150_000_000))
  , payToKey student (ada (Lovelace 50_000_000))
  ]

initializeScholarshipSendScholToSelfTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipSendScholToSelfTx student usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , mintValue (typedPolicyVBT (sSchool scholarship)) () (valueVBT (sSchool scholarship) student (-1))
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) student (-1))
  , payToKey student (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 100_000_000))
  ]

initializeScholarshipSendHalfScholToSelfTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipSendHalfScholToSelfTx student usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , mintValue (typedPolicyVBT (sSchool scholarship)) () (valueVBT (sSchool scholarship) student (-1))
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) student (-1))
  , payToKey student (ada (Lovelace 50_000_000))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student 0)) (ada (Lovelace 50_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 100_000_000))
  ]

initializeScholarshipNoTokensTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipNoTokensTx student _ ref1 ref2 valueIn scholarship  =
  mconcat [
    spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student 0)) (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 100_000_000))
  ]

initializeScholarshipSelfMintedTokensTx :: PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipSelfMintedTokensTx student usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 student ()
  , spendScript (scholarshipPoolScript scholarship) ref2 student ()
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) student (-1))
  , mintValue (typedPolicyVBT student) () (valueVBT student student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student 0)) (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 100_000_000))
  ]

initializeScholarshipWrongTokenNameTx :: PubKeyHash -> PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
initializeScholarshipWrongTokenNameTx attemptedRecipient tokenName usp ref1 ref2 valueIn scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipPoolScript scholarship) ref1 tokenName ()
  , spendScript (scholarshipPoolScript scholarship) ref2 tokenName ()
  , mintValue (typedPolicyVBT (sAuthority scholarship)) () (valueVBT (sAuthority scholarship) tokenName (-1))
  , mintValue (typedPolicyVBT (sSchool scholarship)) () (valueVBT (sSchool scholarship) tokenName (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum attemptedRecipient 0)) (ada (Lovelace 100_000_000))
  , payToScript (scholarshipPoolScript scholarship) (InlineDatum ()) (valueIn - ada (Lovelace 100_000_000))
  ]

refundTx :: PubKeyHash -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx
refundTx refunder ref1 ref2 totalValue scholarship =
  mconcat [
    spendScript (scholarshipPoolScript scholarship) ref1 refunder ()
  , spendScript (scholarshipPoolScript scholarship) ref2 refunder ()
  , payToKey refunder totalValue
  ]

-- ---------------------------------------------------------------------------------------------------
-- -------------------------------------- TESTING SPENDING -------------------------------------------

initializeScholarshipParamed :: (PubKeyHash -> UserSpend -> TxOutRef -> TxOutRef -> Value -> Scholarship -> Tx) -> Integer -> Run ()
initializeScholarshipParamed initializeScholarshipFunction timeToDeadline = do
  -- SETUP USERS
  [donor, auth, school, cp, student] <- setup5Users
  time <- currentTime
  let scholarship = defineScholarship auth school cp 100_000_000 2 (POSIXTime (getPOSIXTime time + 1000000)) -- We define our scholarship for testing purposes to have 100 Ada, 2 milestones, and deadline in 1000 seconds

  -- auth mints their VBT and sends to student
  submitTx auth $ mintSendToken (typedPolicyVBT auth) (valueVBT auth student 1) student    -- Auth submits "mintSendToken" transaction
  -- school mints their VBT and sends to student
  submitTx school $ mintSendToken (typedPolicyVBT school) (valueVBT school student 1) student    -- school submits "mintSendToken" transaction
  -- donor donates 100 ada twice to pool
  donorSpend <- spend donor (ada (Lovelace 200_000_000))
  submitTx donor $ donate scholarship donorSpend

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipFunction student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

initializeScholarshipSelfMintedToken ::  Integer -> Run ()
initializeScholarshipSelfMintedToken timeToDeadline = do
  -- SETUP USERS
  [donor, auth, school, cp, student] <- setup5Users
  time <- currentTime
  let scholarship = defineScholarship auth school cp 100_000_000 2 (POSIXTime (getPOSIXTime time + 1000000)) -- We define our scholarship for testing purposes to have 100 Ada, 2 milestones, and deadline in 1000 seconds

  -- auth mints their VBT and sends to student
  submitTx auth $ mintSendToken (typedPolicyVBT auth) (valueVBT auth student 1) student    -- Auth submits "mintSendToken" transaction
  -- student mints their own (pretending to be school) VBT and sends to student
  submitTx student $ mintSendToken (typedPolicyVBT student) (valueVBT student student 1) student    -- student submits "mintSendToken" transaction
  -- donor donates 100 ada twice to pool
  donorSpend <- spend donor (ada (Lovelace 200_000_000))
  submitTx donor $ donate scholarship donorSpend

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT student student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipSelfMintedTokensTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

initializeScholarshipWrongTokenName ::  Integer -> Run ()
initializeScholarshipWrongTokenName timeToDeadline = do
  -- SETUP USERS
  [donor, auth, school, cp, student] <- setup5Users
  time <- currentTime
  let scholarship = defineScholarship auth school cp 100_000_000 2 (POSIXTime (getPOSIXTime time + 1000000)) -- We define our scholarship for testing purposes to have 100 Ada, 2 milestones, and deadline in 1000 seconds

  -- auth mints their VBT and sends to student
  submitTx auth $ mintSendToken (typedPolicyVBT auth) (valueVBT auth student 1) student    -- Auth submits "mintSendToken" transaction
  -- school mints their VBT and sends to student
  submitTx school $ mintSendToken (typedPolicyVBT school) (valueVBT school student 1) student    -- school submits "mintSendToken" transaction
  -- donor donates 100 ada twice to pool
  donorSpend <- spend donor (ada (Lovelace 200_000_000))
  submitTx donor $ donate scholarship donorSpend

  -- student is tricked into sending both tokens to somoene else (in this case donor, why not)
  studentSpend <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  submitTx student $ sendTo studentSpend (valueVBT auth student 1 <> valueVBT school student 1) donor

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- Donor Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  donorSpendTokens <- spend donor (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipWrongTokenNameTx donor student donorSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

authorityRefundDeadline :: Integer -> Run ()
authorityRefundDeadline timeToDeadline = do
  -- SETUP USERS
  [donor, auth, school, cp, student] <- setup5Users
  time <- currentTime
  let scholarship = defineScholarship auth school cp 100_000_000 2 (POSIXTime (getPOSIXTime time + 1000000)) -- We define our scholarship for testing purposes to have 100 Ada, 2 milestones, and deadline in 1000 seconds

  -- donor donates 100 ada twice to pool
  donorSpend <- spend donor (ada (Lovelace 200_000_000))
  submitTx donor $ donate scholarship donorSpend

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  --Authority Asks for Refund After Deadline Has Passed
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  let [(ref1,out1),(ref2,out2)] = utxos -- We know there are exactly 2
      totalValue = txOutValue out1 <> txOutValue out2
      tx = refundTx auth ref1 ref2 totalValue scholarship
  ct <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx auth tx'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1200_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

nonAuthorityRefundDeadline :: Integer -> Run ()
nonAuthorityRefundDeadline timeToDeadline = do
  -- SETUP USERS
  [donor, auth, school, cp, student] <- setup5Users
  time <- currentTime
  let scholarship = defineScholarship auth school cp 100_000_000 2 (POSIXTime (getPOSIXTime time + 1000000)) -- We define our scholarship for testing purposes to have 100 Ada, 2 milestones, and deadline in 1000 seconds

  -- donor donates 100 ada twice to pool
  donorSpend <- spend donor (ada (Lovelace 200_000_000))
  submitTx donor $ donate scholarship donorSpend

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  --Authority Asks for Refund After Deadline Has Passed
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  let [(ref1,out1),(ref2,out2)] = utxos -- We know there are exactly 2
      totalValue = txOutValue out1 <> txOutValue out2
      tx = refundTx student ref1 ref2 totalValue scholarship
  ct <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx auth tx'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1200_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"
