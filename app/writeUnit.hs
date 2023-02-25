module Main
    ( main
    ) where
        
import Utils                (writeUnit)
import System.Environment   (getArgs)

main :: IO ()
main = do
    [file] <- getArgs
    writeUnit file
