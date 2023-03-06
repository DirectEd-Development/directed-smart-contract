{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module VerifiedByToken where

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


--This minting policy requires that the transaction is signed by the minting institution, and that the token is sent to the pkh specified in the tokenName. Burning is always allowed.
{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy instPkh () ctx = case flattenValue $ txInfoMint txInfo of
                            [(curSym,tn,n)]                                                          --We specifically demand that if any tokens are being minted, there is only a single token being minted/burned. Otherwise not sure how to determine which currencySymbol belongs to this script!
                              | n < 0 -> True --Burning is allowed.    
                              | n > 0 -> traceIfFalse "not signed by institution" signedByInstitution &&
                                          traceIfFalse "must send to specified pkh" sentToNamedWallet --Assumes the tokenName is exactly a pkh, and that the pkh corresponds to the address the token was sent to.                         
                              where
                                txOuts = txInfoOutputs txInfo
                                signedByInstitution = txSignedBy txInfo (unPaymentPubKeyHash instPkh)
                                maybePkhReciever = find (\txOut -> valueOf (txOutValue txOut) curSym tn > 0) txOuts >>= toPubKeyHash . txOutAddress
                                sentToNamedWallet = maybe False ((==) (unTokenName tn) . getPubKeyHash) maybePkhReciever

                            mintingList
                                | and $ (\(_,_,c)->c<0) <$> mintingList  -> True -- Burning everything is allowed.
                                | otherwise -> traceIfFalse "must either burn multiple types of tokens, or mint a single type of token" False

  where
    txInfo = scriptContextTxInfo ctx

{-# INLINABLE policy #-}
policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode pkh

{-# INLINABLE curSymbol #-}
curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type TokenSchema = Endpoint "mint" (Integer, PaymentPubKeyHash)
                   .\/ Endpoint "burn" (Integer, PaymentPubKeyHash)

--As the minting institution, mint tokens and send them to the specified reciever's pkh.
mint :: (Integer,PaymentPubKeyHash) -> Contract w TokenSchema Text ()
mint (n,pkhReciever) = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tn = TokenName $ getPubKeyHash . unPaymentPubKeyHash $ pkhReciever
        val = singleton (curSymbol pkh) tn n
        lookups = Constraints.mintingPolicy $ policy pkh
        tx = Constraints.mustMintValue val <> Constraints.mustPayToPubKey pkhReciever val
    ledgerTx <- adjustAndSubmitWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

--As a token reciever, burn tokens created by the specified minting institution's pkh.
burn :: (Integer, PaymentPubKeyHash) -> Contract w TokenSchema Text ()
burn (n, pkhInstitution) = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tn = TokenName $  getPubKeyHash . unPaymentPubKeyHash $ pkh
        val = singleton (curSymbol pkhInstitution) tn (- n)
        lookups = Constraints.mintingPolicy $ policy pkhInstitution
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
  let w1 = knownWallet 1 --Institution Wallet
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