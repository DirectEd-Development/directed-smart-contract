module Main
    ( main
    ) where

import Data.String ( IsString(..), fromString )
import System.Environment (getArgs)
import Utilities (unsafeTokenNameToHex)
import Plutus.V2.Ledger.Api (TokenName(TokenName), PubKeyHash (getPubKeyHash))

main :: IO ()
main = do
    [receiverPKH] <- getArgs
    let tn = TokenName $ getPubKeyHash $ fromString receiverPKH
    putStrLn $ unsafeTokenNameToHex tn
    
-- Note this spits out the same input as it takes in when it takes a PKH string.

-- This file no longer works for MilestoneToken.