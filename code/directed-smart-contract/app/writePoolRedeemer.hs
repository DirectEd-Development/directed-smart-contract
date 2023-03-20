module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Data.String ( IsString(..), fromString )
import Utilities (writeDataToFile)
import Plutus.V2.Ledger.Api (PubKeyHash)

main :: IO ()
main = do
    [file, receiverPKH] <- getArgs
    writeDataToFile file (fromString receiverPKH :: PubKeyHash)