module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utils                (writeDatum)
import           Cardano.Api                 as API

main :: IO ()
main = do
    [file, addressString, milestone] <- getArgs
    writeDatum file addressString (read milestone)