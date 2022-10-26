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

module Contract where

import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import Plutus.Contract.StateMachine
import Data.Aeson ( FromJSON, ToJSON )
import          GHC.Generics
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada hiding (divide)
import           Ledger.Value           as Value
import           Prelude                (Show (..),Semigroup (..), String)
import qualified Prelude
import Data.Text (Text, pack)
import Plutus.Contract as Contract
import Control.Monad (void)
import qualified VerifiedByToken
import Control.Lens (review)
import Plutus.Contract.Request (mkTxContract)
import Ledger.Typed.Scripts (TypedValidator)

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

data ContractRedeemer = ContractRedeemer {refund :: Bool, emergencyRefund :: Bool} -- The redeemer is used only for refunding. refund can be used once the deadline has passed, and emergencyRefund can be used at any point.
PlutusTx.unstableMakeIsData ''ContractRedeemer

type Milestone = Integer

data ContractDatum = ContractDatum PaymentPubKeyHash Milestone -- The state of the scholarship, whih says who it is for and which milestone they are on. 
PlutusTx.unstableMakeIsData ''ContractDatum

{-# INLINABLE lovelaces #-}
lovelaces :: Ledger.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

--How do we set this up so that we can accept donations and then pool them into one scholarship?
--A: We have two contracts. One for collection of money, and one for the individual scholarships. The collection one must contain the rules for how the individual scholarships are initiated.
--      -For now, we take this approach. In theory this could be combined into one SC if we don't use the StateMachine module, but instead build our own, where the first step of pooling money into a contract isn't tracked by a token.
--How do we set this up so that the fees for minting are paid by the scholarship? (Point for later, don't worry right now)
{-# INLINABLE transition #-}
transition :: Scholarship -> State ContractDatum -> ContractRedeemer -> Maybe (TxConstraints Void Void, State ContractDatum)
transition scholarship State {stateData,stateValue} contractRedeemer = case (stateData, stateValue, contractRedeemer) of
  (ContractDatum pkh _, _, ContractRedeemer _ True)     -> Just ( Constraints.mustBeSignedBy (sAuthority scholarship) 
                                                                , State (ContractDatum pkh $ sMilestones scholarship) mempty ) --"Is this DirectEd asking for the emergencyrefund?"
                                                                                                          --Must ensure this passes the 'final' check so the statemachine is terminated. Hence changing milestones to done (shouldn't really do this).

  (ContractDatum pkh _, _, ContractRedeemer True False)     -> Just ( Constraints.mustBeSignedBy (sAuthority scholarship) 
                                                                    <>Constraints.mustValidateIn (Ledger.from $ sDeadline scholarship)
                                                                    , State (ContractDatum pkh $ sMilestones scholarship) mempty ) --"Is this DirectEd asking for the refund? Have we passed the deadline?"
                                                                                                          --Must ensure this passes the 'final' check so the statemachine is terminated. Hence changing milestones to done (shouldn't really do this).

  (ContractDatum pkh milestone, v, ContractRedeemer False False)
    | milestone < sMilestones scholarship       -> Just ( Constraints.mustBeSignedBy pkh           
                                                        <>Constraints.mustMintValue (singleton (sCourseProviderSym scholarship) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) (-1)) --TokenName should later include milestone number!
                                                        <>Constraints.mustValidateIn (Ledger.to $ sDeadline scholarship)
                                                        , State (ContractDatum pkh $ milestone+1) (v - lovelaceValueOf (divide (sAmount scholarship) $ sMilestones scholarship)))
  --"Signed by pkh, burns milestone token, and next state has Amount/Milestones less value and milestone+1"
  --Note that this assumes the format of the MilestoneToken's TokenName just a pkh

  (ContractDatum pkh milestone, _, ContractRedeemer False False)
    | milestone == sMilestones scholarship      -> Just ( Constraints.mustBeSignedBy pkh
                                                        <>Constraints.mustValidateIn (Ledger.to $ sDeadline scholarship)
                                                        , State (ContractDatum pkh $ milestone+1) mempty ) 
  --"Signed by pkh and next state has empty value" To finish the scholarship.

  _                                              -> Nothing

{-# INLINABLE final #-}
final :: Scholarship -> ContractDatum -> Bool
final scholarship (ContractDatum _ milestone) = sMilestones scholarship == milestone

{-# INLINABLE contractStateMachine #-}
contractStateMachine :: Scholarship -> StateMachine ContractDatum ContractRedeemer
contractStateMachine scholarship = mkStateMachine
    Nothing
    (transition scholarship)
    (final scholarship)

{-# INLINABLE mkContractValidator #-}
mkContractValidator :: Scholarship -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkContractValidator scholarship = mkValidator $ contractStateMachine scholarship

type ContractType = StateMachine ContractDatum ContractRedeemer

typedContractValidator :: Scholarship -> Scripts.TypedValidator ContractType
typedContractValidator scholarship = Scripts.mkTypedValidator @ContractType
    ($$(PlutusTx.compile [|| mkContractValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode scholarship)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @ContractRedeemer

contractValidator :: Scholarship -> Validator
contractValidator = Scripts.validatorScript . Contract.typedContractValidator

contractValHash :: Scholarship -> Ledger.ValidatorHash
contractValHash = Scripts.validatorHash . Contract.typedContractValidator

contractScrAddress ::  Scholarship -> Ledger.Address
contractScrAddress = scriptAddress . contractValidator

contractClient :: Scholarship -> StateMachineClient ContractDatum ContractRedeemer
contractClient scholarship = mkStateMachineClient $ StateMachineInstance (contractStateMachine scholarship) (typedContractValidator scholarship)
--Need to manually implement non-default scChooser in case of multiple of the same contract (although that can come later, as long as it comes!)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

data ScholarshipParams = ScholarshipParams
    { pRecipient        :: !PaymentPubKeyHash
    , pAuthority        :: !PaymentPubKeyHash
    , pAuthoritySym     :: !CurrencySymbol 
    , pSchool           :: !PaymentPubKeyHash 
    , pSchoolSym        :: !CurrencySymbol
    , pCourseProvider   :: !PaymentPubKeyHash
    , pCourseProviderSym:: !CurrencySymbol
    , pAmount           :: !Integer
    , pMilestones       :: !Integer
    , pDeadline         :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

initScholarship :: ScholarshipParams -> Contract () s Text ()
initScholarship sp = do
  let scholarship = Scholarship 
        { sAuthority        = pAuthority sp
        , sAuthoritySym     = pAuthoritySym sp
        , sSchool           = pSchool sp 
        , sSchoolSym        = pSchoolSym sp
        , sCourseProvider   = pCourseProvider sp
        , sCourseProviderSym= pCourseProviderSym sp
        , sAmount           = pAmount sp
        , sMilestones       = pMilestones sp
        , sDeadline         = pDeadline sp
        }
      client = contractClient scholarship
      v = lovelaceValueOf $ sAmount scholarship
  void $ mapError' $ runInitialise client (ContractDatum (pRecipient sp) 0) v 
  logInfo @String "Initialized A Scholarship (using own money)"

initScholarshipManual :: ScholarshipParams -> Contract () s Text ()
initScholarshipManual sp = do
  let scholarship = Scholarship 
        { sAuthority        = pAuthority sp
        , sAuthoritySym     = pAuthoritySym sp
        , sSchool           = pSchool sp 
        , sSchoolSym        = pSchoolSym sp
        , sCourseProvider   = pCourseProvider sp
        , sCourseProviderSym= pCourseProviderSym sp
        , sAmount           = pAmount sp
        , sMilestones       = pMilestones sp
        , sDeadline         = pDeadline sp
        }
      v = lovelaceValueOf $ sAmount scholarship
  void $ mapError' $ runInitialiseNoTT (typedContractValidator scholarship) (ContractDatum (pRecipient sp) 0) v 
  logInfo @String "Initialized A Scholarship (using own money)"

-- | Initialise a state machine and supply additional constraints and lookups for transaction.(Edited from source)
runInitialiseNoTT ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => TypedValidator (StateMachine state input)
    -- ^ The state machine typed validator script
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialiseNoTT typedValidator initialState initialValue = mapError (review _SMContractError) $ do
    ownPK <- Contract.ownPaymentPubKeyHash
    utxo <- utxosAt (Ledger.pubKeyHashAddress ownPK Nothing)
    let constraints = mustPayToTheScript initialState initialValue
        lookups = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs utxo
    utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    -- unless (utx == adjustedUtx) $
    --   logWarn @Text $ "Plutus.Contract.StateMachine.runInitialise: "
    --                 <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
    submitTxConfirmed adjustedUtx
    pure initialState

completeMilestone :: ScholarshipParams -> Contract () s Text ()
completeMilestone sp = do
  time <- Contract.currentTime
  let scholarship = Scholarship 
        { sAuthority        = pAuthority sp
        , sAuthoritySym     = pAuthoritySym sp
        , sSchool           = pSchool sp
        , sSchoolSym        = pSchoolSym sp
        , sCourseProvider   = pCourseProvider sp 
        , sCourseProviderSym= pCourseProviderSym sp
        , sAmount           = pAmount sp
        , sMilestones       = pMilestones sp
        , sDeadline         = pDeadline sp
        }
      client = contractClient scholarship
  if time <= sDeadline scholarship
          then do
            void $ mapError' $ runStepWith (mintingPolicy $ VerifiedByToken.policy $ sCourseProvider scholarship) mempty client $ ContractRedeemer {refund = False, emergencyRefund = False}
            logInfo @String "Burned token and withdrew money" 
          else do
            logInfo @String "Deadline has passed"

refundScholarship :: ScholarshipParams -> Contract () s Text ()
refundScholarship sp = do
  pkh <- Contract.ownPaymentPubKeyHash
  time <- Contract.currentTime
  let scholarship = Scholarship 
        { sAuthority        = pAuthority sp
        , sAuthoritySym     = pAuthoritySym sp
        , sSchool           = pSchool sp 
        , sSchoolSym        = pSchoolSym sp
        , sCourseProvider   = pCourseProvider sp 
        , sCourseProviderSym= pCourseProviderSym sp
        , sAmount           = pAmount sp
        , sMilestones       = pMilestones sp
        , sDeadline         = pDeadline sp
        }
      client = contractClient scholarship
  if pkh == sAuthority scholarship 
    then do
        if time >= sDeadline scholarship
          then do
            void $ mapError' $ runStep client $ ContractRedeemer {refund = False, emergencyRefund = True}
            logInfo @String "Refunded Scholarship to Authority" 
          else do
            logInfo @String "Deadline not passed"
    else do
        logInfo @String "Not authority for scholarship"

type ContractSchema = Endpoint "init" ScholarshipParams .\/ Endpoint "initManual" ScholarshipParams .\/ Endpoint "progress" ScholarshipParams .\/ Endpoint "refund" ScholarshipParams

endpoints :: Contract () ContractSchema Text ()
endpoints = awaitPromise (init `select` initManual `select` progress `select` refund) >> endpoints
  where
    init  = endpoint @"init" initScholarship
    initManual = endpoint @"initManual" initScholarshipManual
    progress = endpoint @"progress" completeMilestone
    refund = endpoint @"refund" refundScholarship

