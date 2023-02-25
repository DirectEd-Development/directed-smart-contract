module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utils                (writeDatum)

main :: IO ()
main = do
    [file, addressString, milestone] <- getArgs
    writeDatum file addressString (read milestone)