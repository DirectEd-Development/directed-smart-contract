-- {-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE DeriveAnyClass      #-}
-- {-# LANGUAGE DeriveGeneric       #-}
-- {-# LANGUAGE FlexibleContexts    #-}
-- {-# LANGUAGE NoImplicitPrelude   #-}
-- {-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell     #-}
-- {-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE TypeOperators       #-}
-- {-# LANGUAGE NumericUnderscores  #-}
-- {-# LANGUAGE NamedFieldPuns      #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Contract where

import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts

-- import           Control.Monad          hiding (fmap)
-- import qualified Data.Map               as Map
-- import           Data.Text              (Text)
-- import           Data.Void              (Void)
-- import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
-- import qualified PlutusTx
-- import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
-- import           Ledger                 hiding (mint, singleton)
-- import           Ledger.Constraints     as Constraints
-- import           Ledger.Ada             as Ada
-- import           Ledger.Contexts        as Contexts
-- import           Ledger.Value           as Value
-- import           Prelude                (IO, Semigroup (..), Show (..), String)
-- import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet
-- import           Test_Token2             as Test_Token
-- import qualified Ledger.Constraints as Constraint
-- import Plutus.Contract.Test.ContractModel (withdraw)

data ContractDatum = False --Placeholder

data ContractRedeemer = True --Placeholder

{-# INLINABLE mkValidator #-}
mkValidator :: ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ = undefined --Placeholder

data ContractType
instance Scripts.ValidatorTypes ContractType where
    type instance DatumType ContractType = ContractDatum
    type instance RedeemerType ContractType = ContractRedeemer

typedValidator :: Scripts.TypedValidator ContractType
typedValidator = undefined --Placeholder

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress ::  Ledger.Address
scrAddress = scriptAddress validator


