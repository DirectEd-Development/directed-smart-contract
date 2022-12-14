{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TestScholarship where

import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import Data.Default               (Default (..))
import PlutusTx.Prelude
import Ledger.TimeSlot (slotToEndPOSIXTime)
import Control.Monad

import Scholarship
import qualified ScholarshipPool
import qualified VerifiedByToken
import Prelude (IO)

basicTest :: IO ()
basicTest = runEmulatorTraceIO basicTrace

basicTrace :: EmulatorTrace ()
basicTrace = do 
    let (w1,w2,w3,w4) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 20

        scholarship = Scholarship
            { sAuthority        = pkh2 
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2 
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3 -- Unused
            , sCourseProvider   = pkh3
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh3
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4p <- activateContractWallet w4 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    
    callEndpoint @"donate" h4p 10_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h4p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

    -- callEndpoint @"mint" h3t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    -- void $ Emulator.waitNSlots 3

    -- callEndpoint @"progress" h1 (ScholarshipDatum pkh1 0)  -- Recipient provides evidence of progress and recieves portion of scholarship.

    -- void $ Emulator.waitNSlots 3

twoStudentsBasicTest :: IO ()
twoStudentsBasicTest = runEmulatorTraceIO twoStudentsBasicTrace

twoStudentsBasicTrace :: EmulatorTrace ()
twoStudentsBasicTrace = do 
    let (w1,w2,w3,w4) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Student 1
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --CourseProvider
        pkh4      = mockWalletPaymentPubKeyHash w4 --Student 2
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 20

        scholarship = Scholarship
            { sAuthority        = pkh2 
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2 
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3 -- Unused
            , sCourseProvider   = pkh3
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh3
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h2 <- activateContractWallet w2 $ endpoints scholarship
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4 <- activateContractWallet w4 $ endpoints scholarship
        
    callEndpoint @"init" h2 pkh1 -- Authority Initiates Scholarship for Recipient 1 (pkh1)

    void $ Emulator.waitNSlots 3

    callEndpoint @"init" h2 pkh4 -- Authority Initiates Scholarship for Recipient 2 (pkh4)

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 (ScholarshipDatum pkh1 0)  -- Recipient 1 provides evidence of progress and recieves portion of scholarship.

    void $ Emulator.waitNSlots 3

refundTest :: IO ()
refundTest = runEmulatorTraceIO refundTrace

refundTrace :: EmulatorTrace ()
refundTrace = do 

    let (w1,w2,w3) = (knownWallet 1,knownWallet 2,knownWallet 3)
    let pkh1      = mockWalletPaymentPubKeyHash w1
        pkh2      = mockWalletPaymentPubKeyHash w2 
        pkh3      = mockWalletPaymentPubKeyHash w3
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 20

        scholarship = Scholarship
            { sAuthority        = pkh2 
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2 
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3 -- Unused
            , sCourseProvider   = pkh3
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh3
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }
    h2 <- activateContractWallet w2 $ endpoints scholarship
       
    callEndpoint @"init" h2 pkh1 -- Authority Initiates Scholarship for Recipient

    void $ Emulator.waitNSlots 20

    callEndpoint @"refund" h2 (ScholarshipDatum pkh1 0)

    void $ Emulator.waitNSlots 3
