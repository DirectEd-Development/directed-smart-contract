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

{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module ScholarshipPool where

import Scholarship
import qualified    PlutusTx
import              Ledger                  hiding (mint, singleton)
import qualified    Ledger.Typed.Scripts    as Scripts
import PlutusTx.Prelude                     hiding (Semigroup(..), unless)
import Ledger.Contexts                      as Contexts
import Ledger.Value                         as Value
import Plutus.Contract                      as Contract
import Data.Text                            (Text)
import           Ledger.Ada             as Ada hiding (divide)


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
mkPoolValidator :: Scholarship -> Ledger.ValidatorHash -> PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPoolValidator schol sValHash pkh _ ctx = traceIfFalse "doesn't consume acceptance token" consumesAcceptaceToken
                                            && traceIfFalse "doesn't reference student status token" referencesStudentToken
                                            && traceIfFalse "doesn't create correct scholarship" createsCorrectScholarship
                                            && traceIfFalse "should create exactly two outputs, the second returning excess ADA to the scholarshipPool script" returnsExcessToScript

    where
        txInfo = scriptContextTxInfo ctx
        valueMinted = txInfoMint txInfo
        consumesAcceptaceToken = valueOf valueMinted (sAuthoritySym schol)
                                (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) == (-1)

        referencesStudentToken = True -- TODO
        outputsAtScholScript = scriptOutputsAt sValHash txInfo -- The outputs at the scholarship script.

        maybeCorrectDatumHash = findDatumHash ( Datum $ PlutusTx.toBuiltinData $ ScholarshipDatum pkh 0) txInfo
        -- The expected datum hash, if it is included in txInfoData in a pair of Datum, DatumHash.

        createsCorrectScholarship = case maybeCorrectDatumHash of
          Nothing -> False
          Just dh -> outputsAtScholScript == [(dh,Ada.lovelaceValueOf $ sAmount schol)]
          -- There should be exactly one output at the scholarship script, with the expected datum and value.

        continuingOutputs = getContinuingOutputs ctx 
        allOutputs = txInfoOutputs txInfo

        returnsExcessToScript = case allOutputs of
          [to1, to2] -> case continuingOutputs of
            [returnTx] -> (returnTx == to1 || returnTx == to2) && isJust (txOutDatum returnTx)
            _ -> False
          _ -> False
        -- Here we demand that there are a total of two outputs, and that one of them returns to the script.
        -- Since we checked earlier that there is an output which creates the correct scholarship, the return output is
        -- the only other output, which means that any excess money withdrawn must be returned to the script. 
        -- Note that we also check that the return output has a datum, to ensure that it is spendable.


data Pool
instance Scripts.ValidatorTypes Pool where
    type instance DatumType Pool = PaymentPubKeyHash
    type instance RedeemerType Pool = ()

typedPoolValidator :: Scholarship -> Scripts.TypedValidator Pool
typedPoolValidator schol = Scripts.mkTypedValidator @Pool
    ($$(PlutusTx.compile [|| mkPoolValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode schol
        `PlutusTx.applyCode` PlutusTx.liftCode (scholarshipValHash schol))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PaymentPubKeyHash @()

poolValidator :: Scholarship -> Validator
poolValidator = Scripts.validatorScript . typedPoolValidator

poolValHash :: Scholarship -> Ledger.ValidatorHash
poolValHash = Scripts.validatorHash . typedPoolValidator

poolScrAddress :: Scholarship -> Ledger.Address
poolScrAddress = scriptAddress . poolValidator

type PoolSchema =
        Endpoint "initScholarship" PaymentPubKeyHash
    .\/ Endpoint "refundPool" ()

--This should:
-- Pick utxos from the pool enough to make a scholarship and send back minScriptAda. (There is theory about how to pick?)
-- Use these to initialize the correct scholarship.
-- Provide 'evidence' - in this case, consumed tokens and referenced tokens - to satisfy the pool script.
initScholarship :: Scholarship -> PaymentPubKeyHash -> Contract () s Text ()
initScholarship scholarship pkhRecipient = do
    let v = lovelaceValueOf $ sAmount scholarship
    utxos <- utxosAt $ scholarshipScrAddress scholarship
    pkhOwn <- Contract.ownPaymentPubKeyHash
    ownUtxos <- utxosAt $ pubKeyHashAddress pkhOwn Nothing
    let acceptanceToken = Value.singleton (sAuthoritySym scholarship) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkhRecipient)) 1
    return () --TODO


-- | Initialise a state machine with no thread token, using a specified set of possible input utxos. 
-- Have to deconstruct and add to initScholarship....
-- runInitialiseNoTT ::
--     forall w e state schema input.
--     ( PlutusTx.FromData state
--     , PlutusTx.ToData state
--     , PlutusTx.ToData input
--     , AsSMContractError e
--     )
--     => Map TxOutRef ChainIndexTxOut
--     -- ^ The set of utxos to use as inputs

--     -> TypedValidator (StateMachine state input)
--     -- ^ The state machine typed validator script
--     -> state
--     -- ^ The initial state
--     -> Value
--     -- ^ The value locked by the contract at the beginning
--     -> Contract w schema e state
-- runInitialiseNoTT utxos typedValidator initialState initialValue = mapError (review _SMContractError) $ do
--     ownPK <- Contract.ownPaymentPubKeyHash
--     let constraints = mustPayToTheScript initialState initialValue
--         lookups = Constraints.typedValidatorLookups typedValidator
--             <> Constraints.unspentOutputs utxos
--     utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
--     let adjustedUtx = Constraints.adjustUnbalancedTx utx
--     -- unless (utx == adjustedUtx) $
--     --   logWarn @Text $ "Plutus.Contract.StateMachine.runInitialise: "
--     --                 <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
--     submitTxConfirmed adjustedUtx
--     pure initialState

refundPool :: Scholarship -> () -> Contract () PoolSchema Text ()
refundPool scholarship () = return () --TODO

poolEndpoints :: Scholarship -> Contract () PoolSchema Text ()
poolEndpoints scholarship = awaitPromise (init' `select` refund') >> poolEndpoints scholarship
  where
    init' = endpoint @"initScholarship" $ initScholarship scholarship
    refund' = endpoint @"refundPool" $ refundPool scholarship

