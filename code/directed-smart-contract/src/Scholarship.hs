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

module Scholarship where
import Plutus.V2.Ledger.Api as PlutusV2
import          GHC.Generics
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Prelude                (Show (..))
import qualified Prelude
import Plutus.V2.Ledger.Api ()
import Plutus.V2.Ledger.Contexts (txSignedBy, findOwnInput, getContinuingOutputs)
import Plutus.V1.Ledger.Value (valueOf)
import Utilities (wrap, validatorHash, validatorHashOld)
import qualified Cardano.Api as Api
import PlutusTx.TH (compile)
import PlutusTx (applyCode, liftCode)

data Scholarship = Scholarship
    { sAuthority        :: PubKeyHash
    , sAuthoritySym     :: CurrencySymbol
    , sSchool           :: PubKeyHash
    , sSchoolSym        :: CurrencySymbol
    , sCourseProvider   :: PubKeyHash
    , sCourseProviderSym:: CurrencySymbol
    , sAmount           :: Integer
    , sMilestones       :: Integer
    , sDeadline         :: POSIXTime
    } deriving (Show, Generic, Prelude.Eq)

PlutusTx.makeLift ''Scholarship

newtype ScholarshipRedeemer = ScholarshipRedeemer {refund :: Bool} -- The redeemer is used only for refunding. refund can be used once the deadline has passed, and emergencyRefund can be used at any point.
PlutusTx.unstableMakeIsData ''ScholarshipRedeemer

type Milestone = Integer

data ScholarshipDatum = ScholarshipDatum PubKeyHash Milestone
  deriving (Show, Generic, Prelude.Eq) -- The state of the scholarship, whih says who it is for and which milestone they are on. 
PlutusTx.unstableMakeIsData ''ScholarshipDatum

--The scholarship contract does the following:
-- cases on sRedeemer (true -> mustbeSignedbyAuthority)
-- false ->
--     cases on milestone number (< milestones -> signed by pkh, burns a token, withinDeadline, creates correctnextState)
--     (= milestones -> signedbyPkh, deadline)
-- otherwise false
{-# INLINABLE mkScholarshipValidator #-}
mkScholarshipValidator :: Scholarship -> ScholarshipDatum -> ScholarshipRedeemer -> ScriptContext -> Bool
mkScholarshipValidator schol (ScholarshipDatum ppkh milestone) sRedeemer ctx = case sRedeemer of
  (ScholarshipRedeemer True) -> traceIfFalse "Refund not signed by authority" $ txSignedBy txInfo (sAuthority schol) --If refunding, must be signed by Authority 

  (ScholarshipRedeemer False)
    | milestone +1 < sMilestones schol -> traceIfFalse "Not signed by recipient" (txSignedBy txInfo ppkh) -- If completing intermediate milestone, must be signed by recipient, 
                                       && traceIfFalse "Milestone token not burned" burnsMilestoneToken --must burn a single milestone token,
                                       && traceIfFalse "Space here for a deadline requirement" True
                                       && traceIfFalse "doesn't withdraw funding correctly" withdrawCorrect --Returns the remaining portion of scholarship to the script, with correct datum.


  (ScholarshipRedeemer False)
    | milestone +1 == sMilestones schol -> traceIfFalse "Not signed by recipient" (txSignedBy txInfo ppkh) -- If completing intermediate milestone, must be signed by recipient, 
                                        && traceIfFalse "Milestone token not burned" burnsMilestoneToken --must burn a single milestone token,
                                        && traceIfFalse "Space here for a deadline requirement" True

  _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    valueMinted = txInfoMint txInfo :: Value
    burnsMilestoneToken = valueOf valueMinted (sCourseProviderSym schol) (TokenName $ consByteString milestone (getPubKeyHash ppkh) ) == (-1)
    scholInputValue = txOutValue . txInInfoResolved <$> findOwnInput ctx
    scholOutputs = getContinuingOutputs ctx
    correctDatum = ScholarshipDatum ppkh (milestone + 1)
    withdrawCorrect = case (scholInputValue, scholOutputs) of
      (Just v, [output]) -> traceIfFalse "incorrect datum, or not inline" ((OutputDatum . Datum . toBuiltinData $ correctDatum) == txOutDatum output)  -- Note this requires there is only a single output, removing the possibility of students banding together to submit a single transaction.
                         && traceIfFalse "incorrect value" (txOutValue output == (v - singleton adaSymbol adaToken (divide (sAmount schol) $ sMilestones schol)))

      _ -> False


mkWrappedScholarshipValidator :: Scholarship -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedScholarshipValidator schol = wrap $ mkScholarshipValidator schol

scholarshipValidator :: Scholarship -> Validator
scholarshipValidator schol = mkValidatorScript ($$(compile [|| mkWrappedScholarshipValidator ||]) `applyCode` liftCode schol )

scholarshipScriptHash :: Scholarship -> Api.ScriptHash
scholarshipScriptHash schol = validatorHash $ scholarshipValidator schol

scholarshipValidatorHash :: Scholarship -> ValidatorHash
scholarshipValidatorHash schol = validatorHashOld $ scholarshipValidator schol