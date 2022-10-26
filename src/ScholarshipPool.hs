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

module ScholarshipPool where

import qualified    Contract
import qualified    PlutusTx
import              Ledger                  hiding (mint, singleton)
import qualified    Ledger.Typed.Scripts    as Scripts
import GHC.Base (undefined)
import PlutusTx.Prelude                     hiding (Semigroup(..), unless)
import Ledger.Contexts                      as Contexts
import Ledger.Value                         as Value
import qualified Plutus.V1.Ledger.Ada as Ada


--Student can consume Acceptance NFT and reference Student Status NFT to create an instance 
-- of the state machine which is their personal scholarship fund.

-- Q: What information is fixed in the contract? Contract.Scholarship
-- Which milestone you start on = 0

-- What information is provided by the redeemer? pkh
-- What information is inferred e.g. from tokens? Nothing, but
-- pkh is checked against tokens and double-checked against signatory

{-# INLINABLE getScriptInputs #-}
-- Gets all transaction inputs that come from the script address being validated
getScriptInputs :: ScriptContext -> [TxInInfo]
getScriptInputs ctx
    | Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput ctx =
    filter (\txInInfo -> Contexts.txOutAddress (Contexts.txInInfoResolved txInInfo) == txOutAddress) (txInfoInputs $ scriptContextTxInfo ctx)
    | Nothing <- findOwnInput ctx = [] --This case should be impossible.

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: Contract.Scholarship -> Ledger.ValidatorHash -> PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPoolValidator schol sValHash pkh _ ctx = traceIfFalse "doesn't consume acceptance token" consumesAcceptaceToken
                                            && traceIfFalse "doesn't reference student status token" referencesStudentToken
                                            && traceIfFalse "doesn't create correct scholarship" createsCorrectScholarship 
                                            && traceIfFalse "doesn't withdraw correct amount" withdrawCorrectAmount
    where
        txInfo = scriptContextTxInfo ctx
        valueMinted = txInfoMint txInfo
        consumesAcceptaceToken = valueOf valueMinted (Contract.sAuthoritySym schol) 
                                (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) == (-1)

        referencesStudentToken = True -- TODO
        outputsAtScholScript = scriptOutputsAt sValHash txInfo -- The outputs at the scholarship script.

        -- correctDatumHash = findDatumHash (PlutusTx.toData $ Contract.ContractDatum pkh 0) 
            --This part seems tricky. HOW TO convert from ContractDatum to Datum??? See BuiltinData section of documentation.
        createsCorrectScholarship = True --TODO
        -- | outputsAtScholScript == [(Contract.ContractDatum pkh 0,Ada.lovelaceValueOf $ Contract.sAmount schol)] = True
        -- | otherwise = False

        scriptInputs = getScriptInputs ctx
        continuingOutputs = getContinuingOutputs ctx --We shall demand exactly 1 continuing Output. (Exactly 1 to avoid the Adjust problem , mandating that there are always 2 total outputs and ensuring they always contain at least minAda from within script)
        valueDeposited = foldMap txOutValue continuingOutputs
        adaDeposited = Ada.fromValue valueDeposited
        withdrawCorrectAmount = True --TODO




data Pool
instance Scripts.ValidatorTypes Pool where
    type instance DatumType Pool = PaymentPubKeyHash
    type instance RedeemerType Pool = ()

typedPoolValidator :: Contract.Scholarship -> Scripts.TypedValidator Pool
typedPoolValidator = undefined

