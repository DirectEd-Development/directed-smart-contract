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

newtype ContractRedeemer = ContractRedeemer {refund :: Bool} -- The only redeemer used is the one which says if the transaction is asking to refund a scholarship with a missed deadline.
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
  (_, _, ContractRedeemer True)                 -> Just ( Constraints.mustBeSignedBy (sAuthority scholarship) 
                                                        , State stateData mempty ) --"Is this DirectEd asking for the refund? In futue version, also ask if it is passed the deadline?"

  (ContractDatum pkh milestone, v, ContractRedeemer False)
    | milestone < sMilestones scholarship       -> Just ( Constraints.mustBeSignedBy pkh           <>
                                                          Constraints.mustMintValue (singleton (sCourseProviderSym scholarship) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) (-1)) --TokenName should later include milestone number?
                                                          , State (ContractDatum pkh $ milestone+1) (v - lovelaceValueOf (divide (sAmount scholarship) $ sMilestones scholarship)))
  --"Signed by pkh, burns milestone token, and next state has Amount/Milestones less value and milestone+1"
  --Note that this assumes the format of the MilestoneToken's TokenName just a pkh

  (ContractDatum pkh milestone, _, ContractRedeemer False)
    | milestone == sMilestones scholarship      -> Just ( Constraints.mustBeSignedBy pkh
                                                          , State (ContractDatum pkh $ milestone+1) mempty ) --"Signed by pkh and next state has empty value" To finish the scholarship.

  _                                             -> Nothing

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

completeMilestone :: ScholarshipParams -> Contract () s Text ()
completeMilestone sp = do
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
  void $ mapError' $ runStepWith (mintingPolicy $ VerifiedByToken.policy $ sCourseProvider scholarship) mempty client $ ContractRedeemer {refund = False}
  logInfo @String "Initialized Personal Scholarship (using own money?)"

refundScholarship :: ScholarshipParams -> Contract () s Text ()
refundScholarship sp = do
  pkh <- Contract.ownPaymentPubKeyHash
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
        void $ mapError' $ runStep client $ ContractRedeemer {refund = True}
        logInfo @String "Refunded Scholarship to Authority" 
    else do
        logInfo @String "Not authority for scholarship"

type ContractSchema = Endpoint "init" ScholarshipParams .\/ Endpoint "progress" ScholarshipParams .\/ Endpoint "refund" ScholarshipParams

endpoints :: Contract () ContractSchema Text ()
endpoints = awaitPromise (init `select` progress `select` refund) >> endpoints
  where
    init  = endpoint @"init" initScholarship
    progress = endpoint @"progress" completeMilestone
    refund = endpoint @"refund" refundScholarship

