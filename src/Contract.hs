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
import          Data.Aeson
import          GHC.Generics
import qualified PlutusTx

-- import           Control.Monad          hiding (fmap)
-- import qualified Data.Map               as Map
-- import           Data.Text              (Text)
-- import           Data.Void              (Void)
-- import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
-- import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
-- import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada hiding (divide)
-- import           Ledger.Contexts        as Contexts
import           Ledger.Value           as Value
-- import           Ledger.Contexts        as Contexts
-- import           Ledger.Value           as Value
import           Prelude                (Show (..),Semigroup (..))
import qualified Prelude
-- import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet
-- import           Test_Token2             as Test_Token
-- import qualified Ledger.Constraints as Constraint
-- import Plutus.Contract.Test.ContractModel (withdraw)
-- import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet
-- import           Test_Token2             as Test_Token
-- import qualified Ledger.Constraints as Constraint
-- import Plutus.Contract.Test.ContractModel (withdraw)
import PlutusTx.Builtins.Class (stringToBuiltinByteString, obfuscatedId)
import AdmissionToken
import Data.Text (pack)


data Scholarship = Scholarship
    { sAuthority        :: !PaymentPubKeyHash
    , sAuthoritySym     :: !CurrencySymbol 
    , sSchoolSym        :: !CurrencySymbol
    , sCourseProviderSym:: !CurrencySymbol
    , sAmount           :: !Integer
    , sMilestones       :: !Integer
    , sDeadline         :: !POSIXTime
    , sToken            :: !ThreadToken
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
--A: We have two contracts. One for collection of money, and one for the individual scholarships. 
--      -For now, we take this approach. In theory this could be combined into one SC if we don't use the StateMachine module, but instead build our own, where the first step of pooling money into a contract isn't tracked by a token.
--How do we set this up so that the fees for minting are paid by the scholarship? (Point for later, don't worry right now)
{-# INLINABLE transition #-}
transition :: Scholarship -> State ContractDatum -> ContractRedeemer -> Maybe (TxConstraints Void Void, State ContractDatum)
transition scholarship State {stateData,stateValue} contractRedeemer = case (stateData, stateValue, contractRedeemer) of
  (_, _, ContractRedeemer True)                 -> Just ( Constraints.mustBeSignedBy (sAuthority scholarship) 
                                                        , State stateData mempty ) --"Is this DirectEd asking for the refund? In futue version, also ask if it is passed the deadline?"

  (ContractDatum pkh milestone, v, ContractRedeemer False)
    | milestone < sMilestones scholarship       -> Just ( Constraints.mustBeSignedBy pkh           <>
                                                          Constraints.mustMintValue (singleton (sCourseProviderSym scholarship) (TokenName $ appendByteString (stringToBuiltinByteString $ show milestone) $ getPubKeyHash (unPaymentPubKeyHash pkh)) (-1)) 
                                                          , State (ContractDatum pkh (milestone+1)) (v - lovelaceValueOf (divide (sAmount scholarship) $ sMilestones scholarship)) )
  --"Signed by pkh, burns milestone token, and next state has Amount/Milestones less value and milestone+1"
  --Note that this assumes the format of the AdmissionToken's TokenName just a pkh
  (ContractDatum pkh milestone, _, ContractRedeemer False)
    | milestone == sMilestones scholarship      -> Just ( Constraints.mustBeSignedBy pkh
                                                          , State (ContractDatum pkh (milestone+1)) mempty ) --"Signed by pkh and next state has empty value"

  _                                             -> Nothing

{-# INLINABLE final #-}
final :: Scholarship -> ContractDatum -> Bool
final scholarship (ContractDatum _ milestone) = sMilestones scholarship == milestone

{-# INLINABLE contractStateMachine #-}
contractStateMachine :: Scholarship -> StateMachine ContractDatum ContractRedeemer
contractStateMachine scholarship = mkStateMachine
    (Just $ sToken scholarship)
    (transition scholarship)
    (final scholarship)

{-# INLINABLE mkContractValidator #-}
mkContractValidator :: Scholarship -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkContractValidator scholarship = mkValidator $ contractStateMachine scholarship

type ContractType = StateMachine ContractDatum ContractRedeemer

--Here is where the error comes from.
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


