{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main where

import           Plutus.Model         (Run,
                                       adaValue, defaultBabbage, mustFail, testNoErrors,
                                       toV2, Tx, payToKey, Ada (Lovelace), ada, newUser, submitTx, valueAt, logError, mintValue, TypedPolicy (TypedPolicy), payToScript, userSpend, UserSpend, TypedValidator (TypedValidator), DatumMode (InlineDatum), spend, currentTime, spendScript, currentTimeRad, validateIn, utxoAt, waitUntil)
import           PlutusTx.Prelude     (($), Integer, consByteString, AdditiveGroup (..))
import           Prelude              (IO, (.), (<>), mconcat, Eq ((==)), (&&), Num ((+)), Bool (False, True))
import           ScholarshipPool
import           VerifiedByToken
import           MilestoneToken
import           Test.Tasty           (defaultMain, testGroup)
import Plutus.V2.Ledger.Api
    ( PubKeyHash(getPubKeyHash),
      Value,
      singleton,
      TokenName(TokenName),
      POSIXTime(POSIXTime, getPOSIXTime),
      TxOutRef,
      TxOut(txOutValue) )
import Control.Monad (replicateM, mapM, unless)
import qualified Scholarship
import Scholarship (Scholarship(..))

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Tests for Scholarship Script"
      [ good "Complete all milestones" (completeMilestones 10000) -- The full inteded flow of the smart contract system
      , bad  "Complete milestone after deadline" (completeMilestones (-10000)) -- Can't complete a milestone after the deadline
      , bad  "Withdraw extra upon completing milestone" (completeFirstMilestoneParamed completeFirstMilestoneWithdrawExtraTx 10000) -- Can't withdraw more than the expected amount from the scholarship when completing a milestone
      , bad  "Withdraw all on non-final milestone" (completeFirstMilestoneParamed completeFirstMilestoneWithdrawAllTx 100000) -- Can't withdraw the entire scholarship when completing a non-final milestone
      , bad  "Skip a milestone" (skipFirstMilestone 100000) -- Can't try to complete a milestone when you have not completed the previous milestone
      , bad  "Complete milestone without token" (completeFirstMilestoneParamed completeFirstMilestoneWithoutToken 100000) -- Can't complete a milestone without burning any token
      , bad  "Complete milestone with wrong tokenName" (completeFirstMilestoneWrongTokenName 100000) -- Can't complete a milestone by burning a token with someone else's tokenName
      , good "Authority Refund Scholarship" (authorityRefundDeadline (-10000)) -- Authority can refund after the deadline
      , bad  "Authority Refund before deadline" (authorityRefundDeadline 10000) -- Authority can't refund before the deadline
      , bad  "Non-authority refund after deadline" (nonAuthorityRefundDeadline (-10000)) -- Non-authority cann't refund after the deadline
      ]
 where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTIONS/INSTANCES/TYPES ------------------------------------

-- Setup many users at once
setup5Users :: Run [PubKeyHash]
setup5Users = replicateM 5 $ newUser (ada (Lovelace 1000_000_000))

valueVBT :: PubKeyHash -> PubKeyHash -> Integer -> Value
valueVBT pkhSender pkhReciever = singleton (VerifiedByToken.curSymbol pkhSender) (TokenName $ getPubKeyHash pkhReciever)

typedPolicyVBT :: PubKeyHash -> TypedPolicy ()
typedPolicyVBT pkhMinter = TypedPolicy (toV2 $ VerifiedByToken.policy pkhMinter)

valueMT :: PubKeyHash -> Integer -> PubKeyHash -> Integer -> Value
valueMT pkhSender milestone pkhReciever = singleton (MilestoneToken.curSymbol pkhSender) (TokenName $ consByteString milestone $ getPubKeyHash pkhReciever)

typedPolicyMT :: PubKeyHash -> TypedPolicy ()
typedPolicyMT pkhMinter = TypedPolicy (toV2 $ MilestoneToken.policy pkhMinter)

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

completeFirstMilestoneTx :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFirstMilestoneTx milestone student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT (sCourseProvider scholarship)) () (valueMT (sCourseProvider scholarship) milestone student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student milestone)) (ada (Lovelace 50_000_000))
  , payToKey student (ada (Lovelace 50_000_000))
  ]

completeFinalMilestoneTx :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFinalMilestoneTx milestone student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT (sCourseProvider scholarship)) () (valueMT (sCourseProvider scholarship) milestone student (-1))
  , payToKey student (ada (Lovelace 50_000_000))
  ]

completeFirstMilestoneWithdrawExtraTx :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFirstMilestoneWithdrawExtraTx milestone student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT (sCourseProvider scholarship)) () (valueMT (sCourseProvider scholarship) milestone student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student milestone)) (ada (Lovelace 20_000_000))
  , payToKey student (ada (Lovelace 80_000_000))
  ]

completeFirstMilestoneWithdrawAllTx :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFirstMilestoneWithdrawAllTx milestone student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT (sCourseProvider scholarship)) () (valueMT (sCourseProvider scholarship) milestone student (-1))
  , payToKey student (ada (Lovelace 100_000_000))
  ]

completeFirstMilestoneWithoutToken :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFirstMilestoneWithoutToken milestone student _ ref1 scholarship  =
  mconcat [
    spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student milestone)) (ada (Lovelace 50_000_000))
  , payToKey student (ada (Lovelace 50_000_000))
  ]

completeFirstMilestoneWrongTokenNameTx :: Integer -> PubKeyHash -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFirstMilestoneWrongTokenNameTx milestone tokenName student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT (sCourseProvider scholarship)) () (valueMT (sCourseProvider scholarship) milestone tokenName (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student milestone)) (ada (Lovelace 50_000_000))
  , payToKey student (ada (Lovelace 50_000_000))
  ]

completeFirstMilestoneSelfMintedTokenTx :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
completeFirstMilestoneSelfMintedTokenTx milestone student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT student) () (valueMT student milestone student (-1))
  , payToScript (scholarshipScript scholarship) (InlineDatum (Scholarship.ScholarshipDatum student milestone)) (ada (Lovelace 50_000_000))
  , payToKey student (ada (Lovelace 50_000_000))
  ]


skipFirstMilestoneTx :: Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx
skipFirstMilestoneTx milestone student usp ref1 scholarship  =
  mconcat [
    userSpend usp
  , spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer False) (Scholarship.ScholarshipDatum student (milestone - 1))
  , mintValue (typedPolicyMT (sCourseProvider scholarship)) () (valueMT (sCourseProvider scholarship) milestone student (-1))
  , payToKey student (ada (Lovelace 100_000_000))
  ]



refundTx :: PubKeyHash -> PubKeyHash -> TxOutRef -> Integer -> Value -> Scholarship -> Tx
refundTx refunder student ref1 milestone totalValue scholarship =
  mconcat [
    spendScript (scholarshipScript scholarship) ref1 (Scholarship.ScholarshipRedeemer True) (Scholarship.ScholarshipDatum student milestone)
  , payToKey refunder totalValue
  ]

-- ---------------------------------------------------------------------------------------------------
-- -------------------------------------- TESTING SPENDING -------------------------------------------

completeMilestones :: Integer -> Run ()
completeMilestones timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- courseProvider mints milestoneTokens and sends to student
  submitTx cp $ mintSendToken (typedPolicyMT cp) (valueMT cp 1 student 1) student    -- cp mints 1st milestone token and sends to student

  -- student completes first milestone by burning token
  utxosMilestoneOne <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens2 <- spend student (valueMT cp 1 student 1)
  let [(ref3, _)] = utxosMilestoneOne                          -- We know there is exactly 1
      txFirstMilestone = completeFirstMilestoneTx 1 student studentSpendTokens2 ref3 scholarship
  ct2  <- currentTimeRad 100
  txFirstMilestone' <- validateIn ct2 txFirstMilestone
  submitTx student txFirstMilestone'

 -- courseProvider mints milestoneTokens and sends to student
  submitTx cp $ mintSendToken (typedPolicyMT cp) (valueMT cp 2 student 1) student    -- cp mints 2nd milestone token and sends to student

  -- student completes first milestone by burning token
  utxosMilestoneTwo <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens3 <- spend student (valueMT cp 2 student 1)
  let [(ref4, _)] = utxosMilestoneTwo                          -- We know there is exactly 1
      txFinalMilestone = completeFinalMilestoneTx 2 student studentSpendTokens3 ref4 scholarship
  ct3  <- currentTimeRad 100
  txFinalMilestone' <- validateIn ct3 txFinalMilestone
  submitTx student txFinalMilestone'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1100_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

completeFirstMilestoneParamed :: (Integer -> PubKeyHash -> UserSpend -> TxOutRef -> Scholarship -> Tx) -> Integer -> Run ()
completeFirstMilestoneParamed completeFirstMilestoneFunction timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- courseProvider mints milestoneTokens and sends to student
  submitTx cp $ mintSendToken (typedPolicyMT cp) (valueMT cp 1 student 1) student    -- cp mints 1st milestone token and sends to student

  -- student completes first milestone by burning token
  utxosMilestoneOne <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens2 <- spend student (valueMT cp 1 student 1)
  let [(ref3, _)] = utxosMilestoneOne                          -- We know there is exactly 1
      txFirstMilestone = completeFirstMilestoneFunction 1 student studentSpendTokens2 ref3 scholarship
  ct2  <- currentTimeRad 100
  txFirstMilestone' <- validateIn ct2 txFirstMilestone
  submitTx student txFirstMilestone'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1050_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

skipFirstMilestone :: Integer -> Run ()
skipFirstMilestone timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

 -- courseProvider mints milestoneToken 2 and sends to student
  submitTx cp $ mintSendToken (typedPolicyMT cp) (valueMT cp 2 student 1) student    -- cp mints 2nd milestone token and sends to student

  -- student attempts to complete last milestone by burning token
  utxosMilestoneTwo <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens3 <- spend student (valueMT cp 2 student 1)
  let [(ref4, _)] = utxosMilestoneTwo                          -- We know there is exactly 1
      txSkipMilestone = skipFirstMilestoneTx 2 student studentSpendTokens3 ref4 scholarship
  ct3  <- currentTimeRad 100
  txSkipMilestone' <- validateIn ct3 txSkipMilestone
  submitTx student txSkipMilestone'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1100_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

completeFirstMilestoneWrongTokenName :: Integer -> Run ()
completeFirstMilestoneWrongTokenName timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- courseProvider mints milestoneTokens and sends to donor (someone other that student)
  submitTx cp $ mintSendToken (typedPolicyMT cp) (valueMT cp 1 donor 1) donor    -- cp mints 1st milestone token and sends to donor

  -- donor now sends that milestone token to student to try to use
  donorSpend2 <- spend donor (valueMT cp 1 donor 1)
  submitTx student $ sendTo donorSpend2 (valueMT cp 1 donor 1) student

  -- student attempts to complete first milestone by burning someone else's (donor's) token
  utxosMilestoneOne <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens2 <- spend student (valueMT cp 1 donor 1)
  let [(ref3, _)] = utxosMilestoneOne                          -- We know there is exactly 1
      txFirstMilestone = completeFirstMilestoneWrongTokenNameTx 1 donor student studentSpendTokens2 ref3 scholarship
  ct2  <- currentTimeRad 100
  txFirstMilestone' <- validateIn ct2 txFirstMilestone
  submitTx student txFirstMilestone'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1050_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

completeFirstMilestoneSelfMintedToken :: Integer -> Run ()
completeFirstMilestoneSelfMintedToken timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  -- student mints their own milestoneTokens
  submitTx student $ mintSendToken (typedPolicyMT student) (valueMT student 1 student 1) student    -- cp mints 1st milestone token and sends to donor

  -- student attempts to complete first milestone by burning tokens they minted
  utxosMilestoneOne <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens2 <- spend student (valueMT student 1 student 1)
  let [(ref3, _)] = utxosMilestoneOne                          -- We know there is exactly 1
      txFirstMilestone = completeFirstMilestoneSelfMintedTokenTx 1 student studentSpendTokens2 ref3 scholarship
  ct2  <- currentTimeRad 100
  txFirstMilestone' <- validateIn ct2 txFirstMilestone
  submitTx student txFirstMilestone'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1000_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1050_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"


authorityRefundDeadline :: Integer -> Run ()
authorityRefundDeadline timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  --Authority Asks for Refund After Deadline Has Passed
  utxosRefund <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  let [(ref3,out3)] = utxosRefund -- We know there are exactly 2
      totalValue = txOutValue out3
      txRefund = refundTx auth student ref3 0 totalValue scholarship
  ct2 <- currentTimeRad 100
  txRefund' <- validateIn ct2 txRefund
  submitTx auth txRefund'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1100_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

nonAuthorityRefundDeadline :: Integer -> Run ()
nonAuthorityRefundDeadline timeToDeadline = do
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

  -- Student Initializes Scholarship by burning tokens 
  utxos <- utxoAt (scholarshipPoolScript scholarship) -- Query blockchain to get all UTxOs at script
  studentSpendTokens <- spend student (valueVBT auth student 1 <> valueVBT school student 1)
  let [(ref1, out1),(ref2, out2)] = utxos                          -- We know there are exactly 2
      valueIn = txOutValue out1 <> txOutValue out2
      tx = initializeScholarshipTx student studentSpendTokens ref1 ref2 valueIn scholarship
  ct  <- currentTimeRad 100
  tx' <- validateIn ct tx
  submitTx student tx'

  --Time Passes
  waitUntil (POSIXTime (getPOSIXTime time + 1000000 - timeToDeadline))

  --Donor Asks for Refund After Deadline Has Passed
  utxosRefund <- utxoAt (scholarshipScript scholarship) -- Query blockchain to get all UTxOs at script
  let [(ref3,out3)] = utxosRefund -- We know there are exactly 2
      totalValue = txOutValue out3
      txRefund = refundTx donor student ref3 0 totalValue scholarship
  ct2 <- currentTimeRad 100
  txRefund' <- validateIn ct2 txRefund
  submitTx donor txRefund'

  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2, v3, v4, v5] <- mapM valueAt [donor, auth, school, cp, student]
  unless (v1 == adaValue 800_000_000 && v2 == adaValue 1100_000_000 && v3 == adaValue 1000_000_000 &&
          v4 == adaValue 1000_000_000 && v5 == adaValue 1000_000_000) $   -- Check if final balances match expected balances
    logError "Final balances are incorrect"

