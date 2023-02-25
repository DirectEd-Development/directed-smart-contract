module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utils                (writeScholRedeemer)

main :: IO ()
main = do
    [filePath, refundString, emergencyRefundString] <- getArgs
    writeScholRedeemer filePath refundString emergencyRefundString