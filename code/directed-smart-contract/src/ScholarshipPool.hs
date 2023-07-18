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

module ScholarshipPool where

import Scholarship
import qualified    PlutusTx
import PlutusTx.Prelude                     hiding (Semigroup(..), unless)
import Control.Lens (none)
import Plutus.V2.Ledger.Api (ScriptContext (scriptContextTxInfo), TxInInfo (..), TxOut (..), ValidatorHash, PubKeyHash (getPubKeyHash), TxInfo (txInfoInputs, txInfoMint, txInfoSignatories), TokenName (TokenName), Datum (Datum), singleton, adaSymbol, adaToken, OutputDatum (OutputDatum, NoOutputDatum), Validator, mkValidatorScript)
import qualified Plutus.V2.Ledger.Contexts as Contexts
import Plutus.V2.Ledger.Contexts (findOwnInput, scriptOutputsAt, getContinuingOutputs)
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx (compile, applyCode, liftCode)
import Utilities (wrap, validatorHash, validatorHashOld)
import qualified Cardano.Api               as Api

-- Student can consume Acceptance NFT and Student Status NFT to create an instance 
-- of the state machine which is their personal scholarship fund.

{-# INLINABLE getScriptInputs #-}
-- | Gets all transaction inputs that come from the script address currently being validated
getScriptInputs :: ScriptContext -> [TxInInfo]
getScriptInputs ctx
    | Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput ctx =
    filter (\txInInfo -> Contexts.txOutAddress (Contexts.txInInfoResolved txInInfo) == txOutAddress) (txInfoInputs $ scriptContextTxInfo ctx)
    | Nothing <- findOwnInput ctx = [] --Note this case should be impossible.

-- The pooling contract allows students to withdraw Ada for their scholarship and put it at the scholarship script, reserved for them. To do this, they must burn one acceptance token and one student token. 
{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: Scholarship -> ValidatorHash -> () -> PubKeyHash -> ScriptContext -> Bool
mkPoolValidator schol sValHash _ pkh ctx = isRefund ||
                                            (traceIfFalse "doesn't consume acceptance token" consumesAcceptaceToken
                                            && traceIfFalse "doesn't consume student status token" consumesStudentToken
                                            && traceIfFalse "doesn't create correct scholarship" createsCorrectScholarship
                                            && traceIfFalse "should return excess ADA to the pool script" returnsExcessToScript)


    where
        txInfo = scriptContextTxInfo ctx
        valueMinted = txInfoMint txInfo
        consumesAcceptaceToken = valueOf valueMinted (sAuthoritySym schol)
                                (TokenName $ getPubKeyHash pkh) == (-1)

        consumesStudentToken = valueOf valueMinted (sSchoolSym schol)
                                (TokenName $ getPubKeyHash pkh) == (-1)

        outputsAtScholScript = scriptOutputsAt sValHash txInfo
        -- The outputs at the scholarship script.

        correctDatum = Datum $ PlutusTx.toBuiltinData $ ScholarshipDatum pkh 0
        -- The correctDatum that should be used in the output UTXO at the scholarship script. (Remember: it is expected to be inline)

        createsCorrectScholarship = outputsAtScholScript == [(OutputDatum correctDatum,singleton adaSymbol adaToken $ sAmount schol)]
          -- There should be exactly one output at the scholarship script, with the expected datum and value. Note that the datum is expected to be inline.

        scriptInputs = getScriptInputs ctx :: [TxInInfo]
        valueWithdrew = foldMap (txOutValue . txInInfoResolved) scriptInputs

        continuingOutputs = getContinuingOutputs ctx :: [TxOut]
        valueDeposited = foldMap txOutValue continuingOutputs
        --We check that all excess ADA withdrawn from the pool is re-deposited to the pool

        withdrawUpToLimit = valueOf (valueWithdrew - valueDeposited) adaSymbol adaToken <= sAmount schol :: Bool
        --We must also check that ada re-deposited has datum (to ensure it is spendable from the script)

        contOutputsHaveDatum = none ((NoOutputDatum ==) . txOutDatum) continuingOutputs

        returnsExcessToScript = traceIfFalse "doesn't withdrawUpToLimit" withdrawUpToLimit && traceIfFalse "continuing outputs don't have datum" contOutputsHaveDatum

        isRefund = (pkh == sAuthority schol) && elem pkh (txInfoSignatories txInfo)
        --If the supplied pkh (in the redeemer) is that of the authority, then this is the authority requesting a full refund in the event of a bug.
        --In this case the transaction must also be signed by that pkh. 


{-# INLINABLE  mkWrappedPoolValidator #-}
mkWrappedPoolValidator :: Scholarship -> ValidatorHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPoolValidator schol valHash = wrap $ mkPoolValidator schol valHash

poolValidator :: Scholarship -> ValidatorHash -> Validator
poolValidator schol valHash = mkValidatorScript ($$(compile [|| mkWrappedPoolValidator ||]) `applyCode` liftCode schol `applyCode` liftCode valHash)

poolScriptHash :: Scholarship -> ValidatorHash -> Api.ScriptHash
poolScriptHash schol valHash = validatorHash $ poolValidator schol valHash

poolValHash :: Scholarship -> ValidatorHash -> ValidatorHash
poolValHash schol valHash = validatorHashOld $ poolValidator schol valHash