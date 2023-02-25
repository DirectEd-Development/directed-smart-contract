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
import Ledger.Typed.Tx (TypedScriptTxOut(tyTxOutData))

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

data ScholarshipRedeemer = ScholarshipRedeemer {refund :: Bool, emergencyRefund :: Bool} -- The redeemer is used only for refunding. refund can be used once the deadline has passed, and emergencyRefund can be used at any point.
PlutusTx.unstableMakeIsData ''ScholarshipRedeemer

type Milestone = Integer

data ScholarshipDatum = ScholarshipDatum PaymentPubKeyHash Milestone
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq) -- The state of the scholarship, whih says who it is for and which milestone they are on. 
PlutusTx.unstableMakeIsData ''ScholarshipDatum

{-# INLINABLE lovelaces #-}
lovelaces :: Ledger.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

--How do we set this up so that we can accept donations and then pool them into one scholarship?
--A: We have two contracts. One for collection of money, and one for the individual scholarships. The collection one must contain the rules for how the individual scholarships are initiated.
{-# INLINABLE transition #-}
transition :: Scholarship -> State ScholarshipDatum -> ScholarshipRedeemer -> Maybe (TxConstraints Void Void, State ScholarshipDatum)
transition scholarship State {stateData,stateValue} sRedeemer = case (stateData, stateValue, sRedeemer) of
  (ScholarshipDatum pkh _, _, ScholarshipRedeemer _ True)     -> Just ( Constraints.mustBeSignedBy (sAuthority scholarship)
                                                                , State (ScholarshipDatum pkh $ 1 + sMilestones scholarship) mempty ) --"Is this DirectEd asking for the emergencyrefund?"
                                                                                                          --Must ensure this passes the 'final' check so the statemachine is terminated. Hence changing milestones to done.

  (ScholarshipDatum pkh _, _, ScholarshipRedeemer True False)     -> Just ( Constraints.mustBeSignedBy (sAuthority scholarship)
                                                                    <>Constraints.mustValidateIn (Ledger.from $ sDeadline scholarship)
                                                                    , State (ScholarshipDatum pkh $ 1 + sMilestones scholarship) mempty ) --"Is this DirectEd asking for the refund? Have we passed the deadline?"
                                                                                                          --Must ensure this passes the 'final' check so the statemachine is terminated. Hence changing milestones to done.

  (ScholarshipDatum pkh milestone, v, ScholarshipRedeemer False False)
    | milestone < sMilestones scholarship       -> Just ( Constraints.mustBeSignedBy pkh
                                                        <>Constraints.mustMintValue (singleton (sCourseProviderSym scholarship) (TokenName $ getPubKeyHash (unPaymentPubKeyHash pkh)) (-1))
                                                        <>Constraints.mustValidateIn (Ledger.to $ sDeadline scholarship)
                                                        , State (ScholarshipDatum pkh $ milestone+1) (v - lovelaceValueOf (divide (sAmount scholarship) $ sMilestones scholarship)))
  --"Signed by pkh, burns milestone token, and next state has Amount/Milestones less value and milestone+1"
  --Note that this assumes the format of the MilestoneToken's TokenName just a pkh

  (ScholarshipDatum pkh milestone, _, ScholarshipRedeemer False False)
    | milestone == sMilestones scholarship      -> Just ( Constraints.mustBeSignedBy pkh
                                                        <>Constraints.mustValidateIn (Ledger.to $ sDeadline scholarship)
                                                        , State (ScholarshipDatum pkh $ milestone+1) mempty )
  --"Signed by pkh and next state has empty value" To finish the scholarship.

  _                                              -> Nothing

{-# INLINABLE final #-}
final :: Scholarship -> ScholarshipDatum -> Bool
final scholarship (ScholarshipDatum _ milestone) = sMilestones scholarship <= milestone

{-# INLINABLE scholarshipStateMachine #-}
scholarshipStateMachine :: Scholarship -> StateMachine ScholarshipDatum ScholarshipRedeemer
scholarshipStateMachine scholarship = mkStateMachine
    Nothing
    (transition scholarship)
    (final scholarship)

{-# INLINABLE mkScholarshipValidator #-}
mkScholarshipValidator :: Scholarship -> ScholarshipDatum -> ScholarshipRedeemer -> ScriptContext -> Bool
mkScholarshipValidator scholarship = mkValidator $ scholarshipStateMachine scholarship

type ScholarshipType = StateMachine ScholarshipDatum ScholarshipRedeemer

typedScholarshipValidator :: Scholarship -> Scripts.TypedValidator ScholarshipType
typedScholarshipValidator scholarship = Scripts.mkTypedValidator @ScholarshipType
    ($$(PlutusTx.compile [|| mkScholarshipValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode scholarship)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ScholarshipDatum @ScholarshipRedeemer

scholarshipValidator :: Scholarship -> Validator
scholarshipValidator = Scripts.validatorScript . typedScholarshipValidator

scholarshipValHash :: Scholarship -> Ledger.ValidatorHash
scholarshipValHash = Scripts.validatorHash . typedScholarshipValidator

scholarshipScrAddress ::  Scholarship -> Ledger.Address
scholarshipScrAddress = scriptAddress . scholarshipValidator

--Need to manually implement non-default scChooser in case of multiple UTXOs at the scholarship script.
--We do this by allowing the user to specify the datum they are looking for: the pkh reciever and milestone they believe it should be on.
--We pick the first utxo with the correct datum, since the datum completely specifies the scholarship state. 
scholarshipChooser :: ScholarshipDatum -> [OnChainState ScholarshipDatum ScholarshipRedeemer] -> Either SMContractError (OnChainState ScholarshipDatum ScholarshipRedeemer)
scholarshipChooser datum states 
  = pickFound $ find (\state -> tyTxOutData (ocsTxOut state) Prelude.== datum) states
  where pickFound Nothing = Left $ ChooserError "No scholarships found with specified datum"
        pickFound (Just state) = Right state

scholarshipClient :: Scholarship -> ScholarshipDatum -> StateMachineClient ScholarshipDatum ScholarshipRedeemer
scholarshipClient scholarship datum = StateMachineClient (StateMachineInstance (scholarshipStateMachine scholarship) (typedScholarshipValidator scholarship)) $ scholarshipChooser datum

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

initScholarshipOwnMoney :: Scholarship -> PaymentPubKeyHash -> Contract () s Text ()
initScholarshipOwnMoney scholarship pkhRecipient = do
  let client = scholarshipClient scholarship 
      v = lovelaceValueOf $ sAmount scholarship
  void $ mapError' $ runInitialise (client (ScholarshipDatum pkhRecipient 0)) (ScholarshipDatum pkhRecipient 0) v --The datum included in client is meaningless here, since we do not search for a utxo
  logInfo @String "Initialized A Scholarship (using own money)"

completeMilestone :: Scholarship -> ScholarshipDatum -> Contract () s Text ()
completeMilestone scholarship expectedDatum = do
  time <- Contract.currentTime
  let client = scholarshipClient scholarship
  if time <= sDeadline scholarship
          then do
            void $ mapError' $ runStepWith (mintingPolicy $ VerifiedByToken.policy $ sCourseProvider scholarship) mempty (client expectedDatum) $ ScholarshipRedeemer {refund = False, emergencyRefund = False}
            logInfo @String "Burned token and withdrew money"
          else do
            logInfo @String "Deadline has passed"

emergencyRefundScholarship :: Scholarship -> ScholarshipDatum -> Contract () s Text ()
emergencyRefundScholarship scholarship expectedDatum = do
  pkh <- Contract.ownPaymentPubKeyHash
  let client = scholarshipClient scholarship
  if pkh == sAuthority scholarship
      then do
        void $ mapError' $ runStep (client expectedDatum) $ ScholarshipRedeemer {refund = False, emergencyRefund = True}
        logInfo @String "Refunded Scholarship to Authority"
    else do
        logInfo @String "Not authority for scholarship"

type ScholarshipSchema =  Endpoint "init" PaymentPubKeyHash
                      .\/ Endpoint "initManual" PaymentPubKeyHash
                      .\/ Endpoint "progress" ScholarshipDatum
                      .\/ Endpoint "emergencyRefund" ScholarshipDatum

endpoints :: Scholarship -> Contract () ScholarshipSchema Text ()
endpoints scholarship = awaitPromise (init `select` progress `select` refund) >> endpoints scholarship
  where
    init  = endpoint @"init" $ initScholarshipOwnMoney scholarship
    progress = endpoint @"progress" $ completeMilestone scholarship
    refund = endpoint @"emergencyRefund" $ emergencyRefundScholarship scholarship
