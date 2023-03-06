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
import           Ledger.Ada                 as Ada hiding (divide)
import           Prelude                    (Show (..),Semigroup (..))
import          GHC.Generics
import qualified Prelude
import Data.Aeson ( FromJSON, ToJSON )
import qualified VerifiedByToken
import Data.Map (toList)
import qualified Ledger.Constraints as Constraints
import Control.Lens (review)
import Plutus.Contract.Request (mkTxContract)
import qualified GHC.Num as Ada
import Plutus.V1.Ledger.Api (ToData(toBuiltinData))
import Ledger.Constraints



-- Student can consume Acceptance NFT and Student Status NFT to create an instance 
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
    | Nothing <- findOwnInput ctx = [] --Note this case should be impossible.

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: Scholarship -> Ledger.ValidatorHash -> () -> PaymentPubKeyHash -> ScriptContext -> Bool
mkPoolValidator schol sValHash _ pkh ctx = isRefund ||
                                            (traceIfFalse "doesn't consume acceptance token" consumesAcceptaceToken
                                            && traceIfFalse "doesn't consume student status token" consumesStudentToken
                                            && traceIfFalse "doesn't create correct scholarship" createsCorrectScholarship
                                            && traceIfFalse "should return excess ADA to the pool script" returnsExcessToScript)




    where
        txInfo = scriptContextTxInfo ctx
        valueMinted = txInfoMint txInfo
        consumesAcceptaceToken = valueOf valueMinted (sAuthoritySym schol)
                                (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) == (-1)

        consumesStudentToken = valueOf valueMinted (sSchoolSym schol)
                                (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) == (-1)

        outputsAtScholScript = scriptOutputsAt sValHash txInfo
        -- The outputs at the scholarship script.

        maybeCorrectDatumHash = findDatumHash ( Datum $ PlutusTx.toBuiltinData $ ScholarshipDatum pkh 0) txInfo
        -- The expected datum hash, if it is included in txInfoData in a pair of Datum, DatumHash.

        createsCorrectScholarship = case maybeCorrectDatumHash of
          Nothing -> False
          Just dh -> outputsAtScholScript == [(dh,Ada.lovelaceValueOf $ sAmount schol)]
          -- There should be exactly one output at the scholarship script, with the expected datum and value.

        scriptInputs = getScriptInputs ctx :: [TxInInfo]
        valueWithdrew = foldMap (txOutValue . txInInfoResolved) scriptInputs
        adaWithdrew = Ada.fromValue valueWithdrew

        continuingOutputs = getContinuingOutputs ctx :: [TxOut]
        valueDeposited = foldMap txOutValue continuingOutputs
        adaDeposited = Ada.fromValue valueDeposited
        --We check that all excess ADA withdrawn from the pool is re-deposited to the pool
        withdrawUpToLimit = adaWithdrew - adaDeposited <= Ada.fromInteger (sAmount schol) :: Bool
        --We must also check that ada re-deposited has isJust datum (to ensure it is spendable from the script)
        contOutputsHaveDatum = all (isJust . txOutDatum) continuingOutputs

        returnsExcessToScript = traceIfFalse "doesn't withdrawUpToLimit" withdrawUpToLimit && traceIfFalse "continuing outputs don't have datum" contOutputsHaveDatum

        --If the supplied pkh (in the redeemer) is that of the authority, then this is the authority requesting a full refund in the event of a bug.
        --In this case the transaction must also be signed by that pkh. 
        isRefund = (pkh == sAuthority schol) && elem pkh (fmap PaymentPubKeyHash (txInfoSignatories txInfo))


data Pool
instance Scripts.ValidatorTypes Pool where
    type instance DatumType Pool = ()
    type instance RedeemerType Pool = PaymentPubKeyHash

typedPoolValidator :: Scholarship -> Scripts.TypedValidator Pool
typedPoolValidator schol = Scripts.mkTypedValidator @Pool
    ($$(PlutusTx.compile [|| mkPoolValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode schol
        `PlutusTx.applyCode` PlutusTx.liftCode (scholarshipValHash schol))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @PaymentPubKeyHash

poolValidator :: Scholarship -> Validator
poolValidator = Scripts.validatorScript . typedPoolValidator

poolValHash :: Scholarship -> Ledger.ValidatorHash
poolValHash = Scripts.validatorHash . typedPoolValidator

poolScrAddress :: Scholarship -> Ledger.Address
poolScrAddress = scriptAddress . poolValidator

type PoolSchema =
        Endpoint "initScholarship" PaymentPubKeyHash
    .\/ Endpoint "refundPool" PaymentPubKeyHash
    .\/ Endpoint "donate" Integer

--To get a Scholarship from PoolParams, we must use the PaymentPubKeyHashes to calculate the currencySymbols of:
-- milestoneToken from sCourseProvider
-- verifiedByToken from sAuthority
-- schoolToken from sSchool
data PoolParams = PoolParams
    { pAuthority        :: !PaymentPubKeyHash
    , pSchool           :: !PaymentPubKeyHash
    , pCourseProvider   :: !PaymentPubKeyHash
    , pAmount           :: !Integer
    , pMilestones       :: !Integer
    , pDeadline         :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

scholarshipFromParams :: PoolParams -> Scholarship
scholarshipFromParams pool =
  Scholarship
  { sAuthority          = pAuthority pool
    , sAuthoritySym     = VerifiedByToken.curSymbol $ pAuthority pool
    , sSchool           = pSchool pool
    , sSchoolSym        = VerifiedByToken.curSymbol $ pSchool pool
    , sCourseProvider   = pCourseProvider pool
    , sCourseProviderSym= VerifiedByToken.curSymbol $ pCourseProvider pool
    , sAmount           = pAmount pool
    , sMilestones       = pMilestones pool
    , sDeadline         = pDeadline pool
    }

poolParamsToScholarship :: Scholarship -> PoolParams
poolParamsToScholarship schol =
    PoolParams
    { pAuthority        = sAuthority schol
    , pSchool           = sSchool schol
    , pCourseProvider   = sCourseProvider schol
    , pAmount           = sAmount schol
    , pMilestones       = sMilestones schol
    , pDeadline         = sDeadline schol
    }

--Utility function that drops elements from a list until dropping one more would cause the predicate to be true. Assumes predicate is false on whole list.
--We will use this function in our off-chain code to select the script UTXOs to spend when creating a scholarship.
dropUntilList :: ([a] -> Bool) -> [a] -> [a]
dropUntilList _ [] = []
dropUntilList p (x:xs)
    | p xs = x:xs
    | otherwise = dropUntilList p xs

--To initialize a scholarship, we do the following:
-- Pick utxos from the pool until we have enough ada to make a scholarship and send back 2 Ada. (Always sending back at least 2 Ada allows us to avoid the attempted construction of a transaction that sends back less than minAda to the script)
-- Use these utxos to initialize the correct scholarship in the scholarship script.
-- Return excess Ada withdrawn to the pool
-- Provide 'evidence' - in this case, consuming an acceptanceToken and a schoolToken - to satisfy the pool script.
initScholarship :: PoolParams -> PaymentPubKeyHash -> Contract () s Text ()
initScholarship pool pkhRecipient = do
    let schol           = scholarshipFromParams pool
        scholValue      = lovelaceValueOf $ sAmount schol
        acceptanceToken = Value.singleton (sAuthoritySym schol) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkhRecipient)) 1
        schoolToken     = Value.singleton (sSchoolSym schol) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkhRecipient)) 1
        scholScriptHash = scholarshipValHash schol
        poolScript      = typedPoolValidator schol
        poolVal         = poolValidator schol
        initialState    = ScholarshipDatum pkhRecipient 0

    utxos <- utxosAt $ poolScrAddress schol

    let utxoList = Data.Map.toList utxos
        utxosToUse = dropUntilList (\list -> leq (sum (txOutValue . toTxOut . snd <$> list)) (scholValue + lovelaceValueOf 2_000_000)) utxoList
        valueToUse = sum (txOutValue . toTxOut . snd <$> utxosToUse)

    let constraints = Constraints.mustPayToOtherScript scholScriptHash (Datum $ toBuiltinData initialState) scholValue
                   <> Constraints.mustPayToTheScript () (valueToUse - scholValue)
                   <> Constraints.mustMintValue (negate acceptanceToken)
                   <> Constraints.mustMintValue (negate schoolToken)
                   <> mconcat [ Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData pkhRecipient | (oref,_) <- utxoList]

        lookups = Constraints.mintingPolicy (VerifiedByToken.policy $ sAuthority schol)
            <> Constraints.mintingPolicy (VerifiedByToken.policy $ sSchool schol)
            <> Constraints.unspentOutputs utxos
            <> Constraints.typedValidatorLookups poolScript
            <> Constraints.otherScript poolVal

    --We then build and submit the transaction based on the above constraints.
    utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    -- unless (utx == adjustedUtx) $
    --   logWarn @Text $ "Plutus.Contract.StateMachine.runInitialise: "
    --                 <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
    submitTxConfirmed adjustedUtx

--This can only be successfully run by the authority.
refundPool :: PoolParams -> PaymentPubKeyHash -> Contract () s Text ()
refundPool pool _ = do
    pkhOwn <- Contract.ownPaymentPubKeyHash
    let schol = scholarshipFromParams pool
        poolScript = typedPoolValidator schol
    utxos <- utxosAt $ poolScrAddress schol
    let utxoList = Data.Map.toList utxos
    let constraints = mconcat [ Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData pkhOwn | (oref,_) <- utxoList]
    let lookups = Constraints.typedValidatorLookups poolScript

    utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx

donate :: PoolParams -> Integer -> Contract () s Text ()
donate pool amount = do
    pkhOwn <- Contract.ownPaymentPubKeyHash
    ownUtxos <- utxosAt $ pubKeyHashAddress pkhOwn Nothing
    let schol = scholarshipFromParams pool
        poolHash = poolValHash schol

    let constraints = Constraints.mustPayToOtherScript poolHash (Datum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf amount)
    let lookups = Constraints.unspentOutputs ownUtxos :: ScriptLookups Pool

    utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    submitTxConfirmed adjustedUtx


poolEndpoints :: PoolParams -> Contract () PoolSchema Text s
poolEndpoints pool = awaitPromise (init' `select` refund' `select` donate') >> poolEndpoints pool
  where
    init' = endpoint @"initScholarship" $ initScholarship pool
    refund' = endpoint @"refundPool" $ refundPool pool
    donate' = endpoint @"donate" $ donate pool

