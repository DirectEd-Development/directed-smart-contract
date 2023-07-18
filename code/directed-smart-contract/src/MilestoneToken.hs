{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module MilestoneToken where

import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx (compile, liftCode, applyCode)
import Utilities ( wrapMintingPolicy, scriptCurrencySymbol )
import Plutus.V1.Ledger.Value (flattenValue, valueOf, toString)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Address (toPubKeyHash)
import Data.String
import Text.Read (readMaybe)
import Data.List (last)
import Data.Char

{-# INLINABLE breakAt #-}
-- Breaks a string into componenets separated by a character. Used assuming tn is in the format pkh#MilestoneNumber
breakAt :: Char -> String -> [String]
breakAt _ [] = []
breakAt a (x:xs) 
  | a == x = []:breakAt a xs
  | otherwise = (x:y):ys
  where
    (y:ys) = breakAt a xs

--This minting policy requires that the transaction is signed by the minting institution, and that the token is sent to the pkh specified in the tokenName. Burning is always allowed.
-- We check the scriptPurpose in order to find the currencySymbol of this script. Then we demand that all minting done with this currencySymbol must either be a burn or signed and sent to specified pkh.
{-# INLINABLE mkPolicy #-}
mkPolicy :: Char -> PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy hashChar instPkh () ctx = all (\(_,tn,n) -> (n<0) || (n>0 && traceIfFalse "not signed by institution" signedByInstitution &&
                                               traceIfFalse "must send to specified pkh" (sentToNamedWallet tn) &&
                                               traceIfFalse "incorrect tn format" (correctTnFormat tn ) )
                                        ) tokensFromScript
  where
    txInfo = scriptContextTxInfo ctx
    txOuts = txInfoOutputs txInfo
    Minting curSym = scriptContextPurpose ctx
    tokensFromScript = filter (\(c,_,_)->c==curSym) (flattenValue $ txInfoMint txInfo)
    signedByInstitution = txSignedBy txInfo instPkh
    maybePkhReciever tn = find (\txOut -> valueOf (txOutValue txOut) curSym tn > 0) txOuts >>= toPubKeyHash . txOutAddress
    tnParts tn = breakAt hashChar (toString tn)
    walletByteString tn = unTokenName . fromString . head . tnParts $ tn -- This could throw errors.
    correctTnFormat tn = isJust (readMaybe . last . tnParts $ tn :: Maybe Integer)

    sentToNamedWallet tn = maybe False ((==) (walletByteString tn ) . getPubKeyHash) (maybePkhReciever tn)

{-# INLINABLE mkWrappedPolicy #-}
mkWrappedPolicy :: Char -> PubKeyHash -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy a = wrapMintingPolicy . mkPolicy a

policy :: Char -> PubKeyHash -> MintingPolicy
policy a pkh = mkMintingPolicyScript ($$(compile [|| mkWrappedPolicy ||]) `applyCode` liftCode a `applyCode` liftCode pkh )

{-# INLINABLE curSymbol #-}
curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . (policy 'b')

