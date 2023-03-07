{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Scholarship where

import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import Data.Aeson ( FromJSON, ToJSON )
import          GHC.Generics
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada hiding (divide)
import           Ledger.Value           as Value
import           Prelude                (Show (..),Semigroup (..), String)
import qualified Prelude
import Data.Text (Text)
import Plutus.Contract as Contract
import qualified VerifiedByToken
import Plutus.V1.Ledger.Api (ToData(toBuiltinData))
import Control.Lens (review)
import Plutus.Contract.Request (mkTxContract)
import qualified Data.Map

data Scholarship = Scholarship
    { sAuthority        :: !PaymentPubKeyHash
    , sAuthoritySym     :: !CurrencySymbol
    , sSchool           :: !PaymentPubKeyHash
    , sSchoolSym        :: !CurrencySymbol
    , sCourseProvider   :: !PaymentPubKeyHash
    , sCourseProviderSym:: !CurrencySymbol
    , sAmount           :: !Integer
    , sMilestones       :: !Integer
    , sDeadline         :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Scholarship

newtype ScholarshipRedeemer = ScholarshipRedeemer {refund :: Bool} -- The redeemer is used only for refunding. refund can be used once the deadline has passed, and emergencyRefund can be used at any point.
PlutusTx.unstableMakeIsData ''ScholarshipRedeemer

type Milestone = Integer

data ScholarshipDatum = ScholarshipDatum PaymentPubKeyHash Milestone
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq) -- The state of the scholarship, whih says who it is for and which milestone they are on. 
PlutusTx.unstableMakeIsData ''ScholarshipDatum

{-# INLINABLE lovelaces #-}
lovelaces :: Ledger.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

--The scholarship contract does the following:
-- cases on sRedeemer (true -> mustbeSignedbyAuthority)
-- false ->
--     cases on milestone number (< milestones -> signed by pkh, burns a token, withinDeadline, creates correctnextState)
--     (= milestones -> signedbyPkh, deadline)
-- otherwise false
{-# INLINABLE mkScholarshipValidator #-}
mkScholarshipValidator :: Scholarship -> ScholarshipDatum -> ScholarshipRedeemer -> ScriptContext -> Bool
mkScholarshipValidator schol (ScholarshipDatum ppkh milestone) sRedeemer ctx = case sRedeemer of
  (ScholarshipRedeemer True) -> traceIfFalse "Refund not signed by authority" $ txSignedBy txInfo (unPaymentPubKeyHash $ sAuthority schol) --If refunding, must be signed by Authority 

  (ScholarshipRedeemer False)
    | milestone < sMilestones schol -> traceIfFalse "Not signed by recipient" (txSignedBy txInfo (unPaymentPubKeyHash ppkh)) -- If completing intermediate milestone, must be signed by recipient, 
                                    && traceIfFalse "Milestone token not burned" burnsMilestoneToken --must burn a single milestone token,
                                    && traceIfFalse "Space here for a deadline requirement" True
                                    && traceIfFalse "withdraws funding correctly" withdrawCorrect --Returns the remaining portion of scholarship to the script, with correct datum.


  (ScholarshipRedeemer False)
    | milestone == sMilestones schol -> traceIfFalse "Not signed by recipient" (txSignedBy txInfo (unPaymentPubKeyHash ppkh)) -- If completing intermediate milestone, must be signed by recipient, 
                                    && traceIfFalse "Milestone token not burned" burnsMilestoneToken --must burn a single milestone token,
                                    && traceIfFalse "Space here for a deadline requirement" True

  _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    valueMinted = txInfoMint txInfo :: Value
    burnsMilestoneToken = valueOf valueMinted (sCourseProviderSym schol) (TokenName $ getPubKeyHash (unPaymentPubKeyHash ppkh)) == (-1)

    scholInputValue = txOutValue . txInInfoResolved <$> findOwnInput ctx
    scholOutputs = getContinuingOutputs ctx
    correctDatum = ScholarshipDatum ppkh (milestone + 1)
    withdrawCorrect = case (scholInputValue, scholOutputs) of
      (Just v, [output]) -> traceIfFalse "incorrect datum" (Just (datumHash . Datum . toBuiltinData $ correctDatum) == txOutDatum output)  -- Note this requires there is only a single output, removing the possibility of students banding together to submit a single transaction.
                         && traceIfFalse "incorrect value" (txOutValue output == (v - lovelaceValueOf (divide (sAmount schol) $ sMilestones schol)))

      _ -> False

data ScholTypes
instance Scripts.ValidatorTypes ScholTypes where
    type instance DatumType ScholTypes = ScholarshipDatum
    type instance RedeemerType ScholTypes = ScholarshipRedeemer

typedScholarshipValidator :: Scholarship -> Scripts.TypedValidator ScholTypes
typedScholarshipValidator schol = Scripts.mkTypedValidator @ScholTypes
    ($$(PlutusTx.compile [|| mkScholarshipValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode schol)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ScholarshipDatum @ScholarshipRedeemer

scholarshipValidator :: Scholarship -> Validator
scholarshipValidator = Scripts.validatorScript . typedScholarshipValidator

scholarshipValHash :: Scholarship -> Ledger.ValidatorHash
scholarshipValHash = Scripts.validatorHash . typedScholarshipValidator

scholarshipScrAddress ::  Scholarship -> Ledger.Address
scholarshipScrAddress = scriptAddress . scholarshipValidator


initScholarshipOwnMoney :: Scholarship -> PaymentPubKeyHash -> Contract () s Text ()
initScholarshipOwnMoney schol pkhRecipient = do
  pkhOwn <- Contract.ownPaymentPubKeyHash
  ownUtxos <- utxosAt $ pubKeyHashAddress pkhOwn Nothing
  let scholValue      = lovelaceValueOf $ sAmount schol
      scholScriptHash = scholarshipValHash schol
      initialState    = ScholarshipDatum pkhRecipient 0

  let constraints = Constraints.mustPayToOtherScript scholScriptHash (Datum $ toBuiltinData initialState) scholValue
      lookups = Constraints.unspentOutputs ownUtxos :: ScriptLookups ScholTypes

  --We then build and submit the transaction based on the above constraints.
  utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
  let adjustedUtx = Constraints.adjustUnbalancedTx utx
  -- unless (utx == adjustedUtx) $
  --   logWarn @Text $ "Plutus.Contract.StateMachine.runInitialise: "
  --                 <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
  submitTxConfirmed adjustedUtx
  logInfo @String "Initialized A Scholarship (using own money)"

completeMilestone :: Scholarship -> ScholarshipDatum -> Contract () s Text ()
completeMilestone schol expectedDatum = do
  let scholScriptHash = scholarshipValHash schol
      scholTypedValidator = typedScholarshipValidator schol
      (ScholarshipDatum pkhRecipient previousMilestone) = expectedDatum
      milestoneToken     = Value.singleton (sCourseProviderSym schol) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkhRecipient)) 1
      oref = _ --Pick one at scholarshipScript with expectedDatum and correct value. 

  allScholarships <- utxosAt $ scholarshipScrAddress schol
  let scholUtxoList = Data.Map.toList allScholarships
      maybeScholUtxo = find (\(txOref, citoTxOut) -> (toTxInfoTxOut citoTxOut)) scholUtxoList
      -- dropUntilList (\list -> leq (sum (txOutValue . toTxOut . snd <$> list)) (scholValue + lovelaceValueOf 2_000_000)) scholUtxoList
      -- valueToUse = sum (txOutValue . toTxOut . snd <$> utxosToUse)

-- pickFound $ find (\state -> tyTxOutData (ocsTxOut state) Prelude.== datum) states
--   where pickFound Nothing = Left $ ChooserError "No scholarships found with specified datum"
--         pickFound (Just state) = Right state

  let lookups           = Constraints.mintingPolicy (VerifiedByToken.policy $ sCourseProvider schol)
                       <> Constraints.typedValidatorLookups scholTypedValidator
      commonConstraints = Constraints.mustMintValue (negate milestoneToken)
                       <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ScholarshipRedeemer False)
                       <> Constraints.mustBeSignedBy pkhRecipient

  if previousMilestone < sMilestones schol
      then let constraints = Constraints.mustPayToOtherScript scholScriptHash (Datum $ toBuiltinData (ScholarshipDatum pkhRecipient (previousMilestone + 1))) (lovelaceValueOf $ divide (sAmount schol) (sMilestones schol) * (sMilestones schol - (previousMilestone + 1)))
                          <> commonConstraints
      in do utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            submitTxConfirmed adjustedUtx
            logInfo @String "Burned token, withdrew next step of money"

      else let constraints = commonConstraints
      in do utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            submitTxConfirmed adjustedUtx
            logInfo @String "Burned token, withdrew rest of money"

refundScholarship :: Scholarship -> ScholarshipDatum -> Contract () s Text ()
refundScholarship schol expectedDatum = do
  pkh <- Contract.ownPaymentPubKeyHash
  let scholTypedValidator = typedScholarshipValidator schol
      oref = _ --Pick one at scholarshipScript with expectedDatum and correct value. 

  let lookups     = Constraints.typedValidatorLookups scholTypedValidator
      constraints = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ScholarshipRedeemer True)
                 <> Constraints.mustBeSignedBy pkh

  if pkh == sAuthority schol
      then do
        utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
        let adjustedUtx = Constraints.adjustUnbalancedTx utx
        submitTxConfirmed adjustedUtx
        logInfo @String "Refunded Scholarship to Authority"
    else do
        logInfo @String "Not authority for scholarship"

type ScholarshipSchema =  Endpoint "init" PaymentPubKeyHash
                      .\/ Endpoint "initManual" PaymentPubKeyHash
                      .\/ Endpoint "progress" ScholarshipDatum
                      .\/ Endpoint "refund" ScholarshipDatum

endpoints :: Scholarship -> Contract () ScholarshipSchema Text ()
endpoints scholarship = awaitPromise (init `select` progress `select` refund) >> endpoints scholarship
  where
    init  = endpoint @"init" $ initScholarshipOwnMoney scholarship
    progress = endpoint @"progress" $ completeMilestone scholarship
    refund = endpoint @"refund" $ refundScholarship scholarship


-- scholarshipChooser :: ScholarshipDatum -> [OnChainState ScholarshipDatum ScholarshipRedeemer] -> Either SMContractError (OnChainState ScholarshipDatum ScholarshipRedeemer)
-- scholarshipChooser datum states 
--   = pickFound $ find (\state -> tyTxOutData (ocsTxOut state) Prelude.== datum) states
--   where pickFound Nothing = Left $ ChooserError "No scholarships found with specified datum"
--         pickFound (Just state) = Right state

