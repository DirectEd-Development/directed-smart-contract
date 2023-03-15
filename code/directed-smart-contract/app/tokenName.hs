module Main
    ( main
    ) where

import Data.String        (IsString (..))
import System.Environment (getArgs)
import Utilities
import Utils              (unsafeTokenNameToHex, stringToPPKH)
import Ledger.Crypto      (PubKeyHash(getPubKeyHash))
import Ledger.Value       (TokenName(TokenName))

main :: IO ()
main = do
    [receiverAddr] <- getArgs
    let tn = TokenName $ getPubKeyHash . unPaymentPubKeyHash $ stringToPPKH receiverAddr
    putStrLn $ unsafeTokenNameToHex tn

