{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Plutus.Model         (Run,
                                       adaValue, defaultBabbage, mustFail, testNoErrors,
                                       toV2, Tx, payToKey, Ada (Lovelace), ada, newUser, submitTx, valueAt, logError, mintValue, TypedPolicy (TypedPolicy))
import           PlutusTx.Prelude     (($), Integer, consByteString)
import           Prelude              (IO, (.), (<>), mconcat, Eq ((==)), (&&))
import           MilestoneToken
import           Test.Tasty           (defaultMain, testGroup)
import Plutus.V1.Ledger.Api (PubKeyHash (getPubKeyHash), Value, singleton, TokenName (TokenName))
import Control.Monad (replicateM, mapM, unless)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Tests for Milestone Token Minting Policy"
      [ good "Normal minting" normalMintToken -- Can mint as intended
      , bad  "Minting with wrong signature" wrongMinter -- Only the minting institution can mint
      , bad  "Minting and sending to wrong wallet" wrongRecipientMint -- Must send minted tokens to the wallet specified in the pkh in the tokenName
      ]
 where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTIONS/INSTANCES/TYPES ------------------------------------

-- Set many users at once
setup2Users :: Run [PubKeyHash]
setup2Users = replicateM 2 $ newUser (ada (Lovelace 1000))

valueMT :: PubKeyHash -> Integer -> PubKeyHash -> Integer -> Value
valueMT pkhSender milestone pkhReciever = singleton (MilestoneToken.curSymbol pkhSender) (TokenName $ consByteString milestone $ getPubKeyHash pkhReciever)

typedPolicyMT :: PubKeyHash -> TypedPolicy ()
typedPolicyMT pkhMinter = TypedPolicy (toV2 $ MilestoneToken.policy pkhMinter)

mintSendToken :: TypedPolicy () -> Value -> PubKeyHash -> Tx
mintSendToken typedPolicy valueMint pkhRecipient =
  mconcat
    [ mintValue typedPolicy () valueMint
    , payToKey pkhRecipient valueMint
    ]

-- ---------------------------------------------------------------------------------------------------
-- -------------------------------------- TESTING SPENDING -------------------------------------------

normalMintToken :: Run ()
normalMintToken = do
  -- SETUP USERS
  [auth, student] <- setup2Users
  -- auth mints their MT and sends to student
  submitTx auth $ mintSendToken (typedPolicyMT auth) (valueMT auth 1 student 1) student    -- User 2 submits "mintMT" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [auth, student]                     -- Get final balances of both users
  unless (v1 == adaValue 1000 && v2 == (adaValue 1000 <> valueMT auth 1 student 1)) $  -- Check if final balances match expected balances
    logError "Final balances are incorrect"

wrongMinter :: Run ()
wrongMinter = do
  -- SETUP USERS
  [auth, student] <- setup2Users
  -- student tries to mint auth's MT and send to themselves
  submitTx student $ mintSendToken (typedPolicyMT auth) (valueMT auth 1 student 1) student    -- User 1 submits "mintMT" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [auth, student]                     -- Get final balances of both users
  unless (v1 == adaValue 1000 && v2 == (adaValue 1000 <> valueMT auth 1 student 1)) $  -- Check if final balances match expected balances
    logError "Final balances are incorrect"

wrongRecipientMint :: Run ()
wrongRecipientMint = do
  -- SETUP USERS
  [auth, student] <- setup2Users
  -- auth tries to mint their MT and send to themselves
  submitTx auth $ mintSendToken (typedPolicyMT auth) (valueMT auth 1 student 1) auth    -- User 1 submits "mintMT" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [auth, student]                     -- Get final balances of both users
  unless (v1 == adaValue 1000  <> valueMT auth 1 student 1 && v2 == adaValue 1000) $  -- Check if final balances match expected balances
    logError "Final balances are incorrect"
  
