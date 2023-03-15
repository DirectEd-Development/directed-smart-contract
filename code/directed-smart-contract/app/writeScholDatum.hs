module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Scholarship          (ScholarshipDatum (ScholarshipDatum))
import Utilities (writeDataToFile)
import Data.String (IsString(fromString))

main :: IO ()
main = do
    [file, pkhString, milestone] <- getArgs
    writeDataToFile file $ Scholarship.ScholarshipDatum (fromString pkhString) (read milestone)