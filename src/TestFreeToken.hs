{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module TestFreeToken where

import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified PlutusTx
import Ledger.Value as Value ( singleton, TokenName (TokenName) )
import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import           Ledger.Constraints     as Constraints
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import Data.Maybe (fromJust)


--The free minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

{-# INLINABLE policy #-}
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])


{-# INLINABLE curSymbol #-}
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol  policy


--Offchain code from VerifiedByToken
type TokenSchema = Endpoint "mint" (Integer, PaymentPubKeyHash)
                   .\/ Endpoint "burn" (Integer, PaymentPubKeyHash)

--As the minting authority, mint tokens and send them to the specified reciever's pkh.
mint :: (Integer,PaymentPubKeyHash) -> Contract w TokenSchema Text ()
mint (n,pkhReciever) = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tn = TokenName $ getPubKeyHash . unPaymentPubKeyHash $ pkhReciever
        val = singleton curSymbol tn n
        lookups = Constraints.mintingPolicy policy
        tx = Constraints.mustMintValue val <> Constraints.mustPayToPubKey pkhReciever val
    ledgerTx <- adjustAndSubmitWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

--As a token reciever, burn tokens created by the specified minting authority's pkh.
burn :: (Integer, PaymentPubKeyHash) -> Contract w TokenSchema Text ()
burn (n, pkhAuthority) = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tn = TokenName $  getPubKeyHash . unPaymentPubKeyHash $ pkh
        val = singleton (curSymbol) tn (- n)
        lookups = Constraints.mintingPolicy $ policy
        tx = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed

endpoints :: Contract () TokenSchema Text ()
endpoints = awaitPromise (mint' `select` burn') >> endpoints
    where
        mint' = endpoint @"mint" mint
        burn' = endpoint @"burn" burn

test :: IO ()
test = runEmulatorTraceIO $ do
  let w1 = knownWallet 1 --Authority Wallet
  let w2 = knownWallet 2 --Reciever Wallet
  let w1State = emptyWalletState w1
  let pkh1 = PaymentPubKeyHash $ fromJust (w1State >>= toPubKeyHash . ownAddress)
  let w2State = emptyWalletState w2
  let pkh2 = PaymentPubKeyHash $ fromJust (w2State >>= toPubKeyHash . ownAddress)
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  callEndpoint @"mint" h1 (1,pkh2) --Mint a token and send to pkh2
  void $ Emulator.waitNSlots 1
  callEndpoint @"burn" h2 (1,pkh1) --Burn the token
  void $ Emulator.waitNSlots 1