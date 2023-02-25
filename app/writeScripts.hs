module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utils                (writeDatum, constructScholarship, writePolicies)
import           Cardano.Api                 as API

main :: IO (Either (FileError ()) ())
main = do
    [auth, school, courseP, amount, milestones, deadline] <- getArgs
    let scholarship = constructScholarship auth school courseP (read amount) (read milestones) (read deadline)
    writePolicies "poolValidator.script" "scholarshipValidator.script" "acceptancePolicy.script" "milestonePolicy.script" scholarship