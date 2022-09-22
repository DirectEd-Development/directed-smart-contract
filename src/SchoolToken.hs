{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module SchoolToken where

import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified PlutusTx
import Ledger.Value as Value ( flattenValue, valueOf, singleton, TokenName (unTokenName, TokenName) )
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


--This minting policy requires that the transaction is signed by the minting authority, and that the token is sent to the pkh specified in the tokenName. Burning is always allowed.
{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy authPkh () ctx = case flattenValue $ txInfoMint txInfo of
                            [(curSym,tn,n)]                                                          --We specifically demand that there is exactly one token being minted/burned so that we know it is the one from this script. Not sure how else to determine which token comes from this script? 
                              | n < 0 -> True --Burning is allowed.    
                              | n > 0 -> traceIfFalse "not signed by authority" signedByAuthority &&
                                          traceIfFalse "must send to specified pkh" sentToNamedWallet --Checks format of the tokenname is 'SchoolToken:<pkh>' and that the pkh corresponds to the address the token was sent to.                         
                              where
                                txOuts = txInfoOutputs txInfo
                                signedByAuthority = txSignedBy txInfo (unPaymentPubKeyHash authPkh)
                                maybePkhReciever = find (\txOut -> valueOf (txOutValue txOut) curSym tn > 0) txOuts >>= toPubKeyHash . txOutAddress
                                sentToNamedWallet = maybe False ((\pkh -> mappend "SchoolToken:" pkh == unTokenName tn) . getPubKeyHash) maybePkhReciever

                            _         -> traceIfFalse "Must mint/burn exactly one type of token" False
  where
    txInfo = scriptContextTxInfo ctx

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type TokenSchema = Endpoint "mint" (Integer, PaymentPubKeyHash)
                   .\/ Endpoint "burn" (Integer, PaymentPubKeyHash)

--As the minting authority, mint tokens and send them to the specified reciever's pkh.
mint :: (Integer,PaymentPubKeyHash) -> Contract w TokenSchema Text ()
mint (n,pkhReciever) = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tn = TokenName $  mappend "SchoolToken:" $ getPubKeyHash . unPaymentPubKeyHash $ pkhReciever
        val = singleton (curSymbol pkh) tn n
        lookups = Constraints.mintingPolicy $ policy pkh
        tx = Constraints.mustMintValue val <> Constraints.mustPayToPubKey pkhReciever val
    ledgerTx <- adjustAndSubmitWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

--As a token reciever, burn tokens created by the specified minting authority's pkh.
burn :: (Integer, PaymentPubKeyHash) -> Contract w TokenSchema Text ()
burn (n, pkhAuthority) = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tn = TokenName $  getPubKeyHash . unPaymentPubKeyHash $ pkh
        val = singleton (curSymbol pkhAuthority) tn (- n)
        lookups = Constraints.mintingPolicy $ policy pkhAuthority
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