{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module VerifiedByToken where

import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx (compile, liftCode, applyCode)
import Utilities (wrapMintingPolicy)
import Plutus.V1.Ledger.Value (flattenValue, valueOf)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Address (toPubKeyHash)
-- import              Ledger                  hising (mint, singleton)
-- import qualified    Ledger.Typed.Scripts    as Scripts
-- import Ledger.Contexts                      as Contexts
-- import Ledger.Value                         as Value
-- import           Ledger.Ada                 as Ada hiding (divide)


--This minting policy requires that the transaction is signed by the minting institution, and that the token is sent to the pkh specified in the tokenName. Burning is always allowed.
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy instPkh () ctx = case flattenValue $ txInfoMint txInfo of
                            [(curSym,tn,n)]                                                          --We specifically demand that if any tokens are being minted, there is only a single token being minted/burned. Otherwise not sure how to determine which currencySymbol belongs to this script!
                              | n < 0 -> True --Burning is allowed.    
                              | n > 0 -> traceIfFalse "not signed by institution" signedByInstitution &&
                                          traceIfFalse "must send to specified pkh" sentToNamedWallet --Assumes the tokenName is exactly a pkh, and that the pkh corresponds to the address the token was sent to.                         
                              where
                                txOuts = txInfoOutputs txInfo
                                signedByInstitution = txSignedBy txInfo instPkh
                                maybePkhReciever = find (\txOut -> valueOf (txOutValue txOut) curSym tn > 0) txOuts >>= toPubKeyHash . txOutAddress
                                sentToNamedWallet = maybe False ((==) (unTokenName tn) . getPubKeyHash) maybePkhReciever

                            mintingList
                                | and $ (\(_,_,c)->c<0) <$> mintingList  -> True -- Burning everything is allowed.
                                | otherwise -> traceIfFalse "must either burn multiple types of tokens, or mint a single type of token" False

  where
    txInfo = scriptContextTxInfo ctx

{-# INLINABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PubKeyHash -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapMintingPolicy . mkPolicy

policy :: PubKeyHash -> MintingPolicy
policy pkh = mkMintingPolicyScript ($$(compile [|| mkWrappedPolicy ||]) `applyCode` liftCode pkh)

-- {-# INLINABLE curSymbol #-}
-- curSymbol :: PubKeyHash -> CurrencySymbol
-- curSymbol = scriptCurrencySymbol . policy


--validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
