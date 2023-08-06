module Main
    ( main
    ) where

import System.Environment   (getArgs)
import Scholarship          (ScholarshipRedeemer (ScholarshipRedeemer))
import Utilities (writeDataToFile)
import Plutus.V1.Ledger.Api (toData)

main :: IO ()
main = do
    [file, refundString] <- getArgs
    writeDataToFile file $ Scholarship.ScholarshipRedeemer $ read refundString
    print(toData $ ScholarshipRedeemer False)
