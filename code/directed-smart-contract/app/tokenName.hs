{-# LANGUAGE OverloadedStrings   #-}

module Main
    ( main
    ) where

import Data.String ( IsString(..), fromString )
import System.Environment (getArgs)
import Utilities (unsafeTokenNameToHex)
import Plutus.V2.Ledger.Api (TokenName(TokenName, unTokenName), PubKeyHash (getPubKeyHash))
import PlutusTx.Builtins (lengthOfByteString, consByteString, indexByteString, sliceByteString)
import Plutus.V1.Ledger.Value (toString)

main :: IO ()
main = do
    [receiverPKH] <- getArgs
    let tn = TokenName $ getPubKeyHash $ fromString receiverPKH
    let tnByteStr1 = unTokenName tn
    let tnByteStr = consByteString 257 tnByteStr1
    putStrLn "tn as TokenName"
    print tn
    putStrLn "tn as String"
    print $ toString tn
    putStrLn "tn as BuiltinByteString"
    print tnByteStr
    print (lengthOfByteString tnByteStr)
    putStrLn "tn as Hex"
    putStrLn $ unsafeTokenNameToHex tn
    putStrLn "First Index"
    print $ indexByteString tnByteStr 0
    print $ sliceByteString 1 28 tnByteStr

-- Note this spits out the same input as it takes in when it takes a PKH string.

-- This file no longer works for MilestoneToken.    