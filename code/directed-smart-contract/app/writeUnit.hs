module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utilities (writeDataToFile)

main :: IO ()
main = do
    [file] <- getArgs
    writeDataToFile file ()