module Main
    ( main
    ) where
        
import System.Environment   (getArgs)
import Utilities            (writeValidatorToFile)
import Scholarship
import qualified ScholarshipPool
import qualified VerifiedByToken
import Data.String (IsString(fromString))
import Plutus.V2.Ledger.Api (POSIXTime(POSIXTime), Validator (Validator), MintingPolicy (getMintingPolicy))
import qualified MilestoneToken

main :: IO ()
main = do
    [authPKH, schoolPKH, coursePPKH, amount, milestones, deadline] <- getArgs
    let scholarship = constructScholarship authPKH schoolPKH coursePPKH (read amount) (read milestones) (read deadline)
    writePolicies "poolValidator.script" "scholarshipValidator.script" "schoolPolicy.script" "acceptancePolicy.script" "milestonePolicy.script" scholarship

constructScholarship :: String -> String -> String -> Integer -> Integer -> Integer -> Scholarship.Scholarship
constructScholarship auth school courseProvider amount milestones deadline = 
  Scholarship.Scholarship {
        sAuthority      = fromString auth
      , sAuthoritySym   = VerifiedByToken.curSymbol $ fromString auth 
      , sSchool         = fromString school
      , sSchoolSym      = VerifiedByToken.curSymbol $ fromString school  
      , sCourseProvider = fromString courseProvider
      , sCourseProviderSym = MilestoneToken.curSymbol $ fromString courseProvider   
      , sAmount         = amount                --For now 100_000_000
      , sMilestones     = milestones            --For now 2
      , sDeadline       = POSIXTime deadline
      }

writeScholarshipValidator :: FilePath -> Scholarship.Scholarship -> IO ()
writeScholarshipValidator filePath scholarship = 
  writeValidatorToFile filePath $ Scholarship.scholarshipValidator scholarship

writePoolValidator :: FilePath -> Scholarship.Scholarship -> IO ()
writePoolValidator filePath scholarship = 
  writeValidatorToFile filePath $ ScholarshipPool.poolValidator scholarship (scholarshipValidatorHash scholarship)

writeSchoolPolicy :: FilePath -> Scholarship.Scholarship -> IO ()
writeSchoolPolicy filePath scholarship = writeValidatorToFile filePath $ Validator $ getMintingPolicy $ VerifiedByToken.policy (Scholarship.sSchool scholarship) 

writeAcceptancePolicy :: FilePath -> Scholarship.Scholarship -> IO ()
writeAcceptancePolicy filePath scholarship = writeValidatorToFile filePath $ Validator $ getMintingPolicy $ VerifiedByToken.policy (Scholarship.sAuthority scholarship) 

writeMilestonePolicy :: FilePath -> Scholarship.Scholarship -> IO ()
writeMilestonePolicy filePath scholarship = writeValidatorToFile filePath $ Validator $ getMintingPolicy $ MilestoneToken.policy (Scholarship.sCourseProvider scholarship) 

writePolicies :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Scholarship.Scholarship -> IO ()
writePolicies poolPath scholPath schoolTokenPath acceptanceTokenPath milesTokenPath scholarship = do
  _ <- writePoolValidator poolPath scholarship
  _ <- writeSchoolPolicy schoolTokenPath scholarship
  _ <- writeScholarshipValidator scholPath scholarship
  _ <- writeAcceptancePolicy acceptanceTokenPath scholarship
  writeMilestonePolicy milesTokenPath scholarship 
