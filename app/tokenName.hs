module Main
    ( main
    ) where

import Data.String        (IsString (..))
import System.Environment (getArgs)
import Utils              (unsafeTokenNameToHex, stringToPPKH)
import Ledger.Address     (PaymentPubKeyHash(unPaymentPubKeyHash) )
import Ledger.Crypto      (PubKeyHash(getPubKeyHash))
import Ledger.Value       (TokenName(TokenName))


main :: IO ()
main = do
    [receiverAddr] <- getArgs
    let tn = TokenName $ getPubKeyHash . unPaymentPubKeyHash $ stringToPPKH receiverAddr
    putStrLn $ unsafeTokenNameToHex tn

