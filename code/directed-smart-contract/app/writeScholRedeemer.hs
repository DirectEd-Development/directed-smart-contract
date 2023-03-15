module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Scholarship          (ScholarshipRedeemer (ScholarshipRedeemer))
import Utilities (writeDataToFile)

main :: IO ()
main = do
    [file, refundString] <- getArgs
    writeDataToFile file $ Scholarship.ScholarshipRedeemer $ read refundString