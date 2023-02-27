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
import Wallet.Emulator.MultiAgent (EmulatorState)

testStep1 :: IO ()
testStep1 = runEmulatorTraceIO traceStep1

traceStep1 :: EmulatorTrace ()
traceStep1 = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        _         = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

testStep2 :: IO ()
testStep2 = runEmulatorTraceIO traceStep2

traceStep2 :: EmulatorTrace ()
traceStep2 = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        _         = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

testStep3 :: IO ()
testStep3 = runEmulatorTraceIO traceStep3

traceStep3 :: EmulatorTrace ()
traceStep3 = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        _         = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

testStep4 :: IO ()
testStep4 = runEmulatorTraceIO traceStep4

traceStep4 :: EmulatorTrace ()
traceStep4 = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        _         = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h4t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

testStep5 :: IO ()
testStep5 = runEmulatorTraceIO traceStep5

traceStep5 :: EmulatorTrace ()
traceStep5 = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        _         = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h4t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 (ScholarshipDatum pkh1 0)  -- Recipient provides evidence of progress and recieves portion of scholarship.

    void $ Emulator.waitNSlots 3

testStep6 :: IO ()
testStep6 = runEmulatorTraceIO traceStep6

traceStep6 :: EmulatorTrace ()
traceStep6 = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        _         = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h4t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 (ScholarshipDatum pkh1 0)  -- Recipient provides evidence of progress and recieves portion of scholarship.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 (ScholarshipDatum pkh1 1) -- Recipient provides further evidence of progress and recieves the second portion of the scholarship.

    void $ Emulator.waitNSlots 3

twoStudentsBasicTest :: IO ()
twoStudentsBasicTest = runEmulatorTraceIO twoStudentsBasicTrace

twoStudentsBasicTrace :: EmulatorTrace ()
twoStudentsBasicTrace = do
    let (w1,w2,w3,w4,w5,w6) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5,knownWallet 6)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        pkh5      = mockWalletPaymentPubKeyHash w5 --Donor
        pkh6      = mockWalletPaymentPubKeyHash w6 --Recipient 2
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h6 <- activateContractWallet w6 $ endpoints scholarship
    h6p <- activateContractWallet w6 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh6) -- Authority mints 1 acceptance token and sends to recipient 2.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh6) -- School mints 1 school token and sends to recipient 2.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h6p pkh6 -- Recipient 2 initiates personal scholarship

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h4t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h4t (2,pkh6) -- Courseprovider mints 2 milestone completion tokens and sends to recipient 2.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 (ScholarshipDatum pkh1 0)  -- Recipient provides evidence of progress and recieves portion of scholarship.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h6 (ScholarshipDatum pkh6 0)  -- Recipient 2 provides evidence of progress and recieves portion of scholarship.

    void $ Emulator.waitNSlots 3


refundTest :: IO ()
refundTest = runEmulatorTraceIO refundTrace

refundTrace :: EmulatorTrace ()
refundTrace = do
    let (w1,w2,w3,w4,w5) = (knownWallet 1,knownWallet 2,knownWallet 3,knownWallet 4,knownWallet 5)
    let pkh1      = mockWalletPaymentPubKeyHash w1 --Recipient
        pkh2      = mockWalletPaymentPubKeyHash w2 --Authority
        pkh3      = mockWalletPaymentPubKeyHash w3 --School
        pkh4      = mockWalletPaymentPubKeyHash w4 --Course Provider     
        pkh5      = mockWalletPaymentPubKeyHash w5 --Donor
        amount    = 40_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 40

        scholarship = Scholarship
            { sAuthority        = pkh2
            , sAuthoritySym     = VerifiedByToken.curSymbol pkh2
            , sSchool           = pkh3
            , sSchoolSym        = VerifiedByToken.curSymbol pkh3
            , sCourseProvider   = pkh4
            , sCourseProviderSym= VerifiedByToken.curSymbol pkh4
            , sAmount           = amount
            , sMilestones       = milestones
            , sDeadline         = deadline
            }

    h1 <- activateContractWallet w1 $ endpoints scholarship
    h1p <- activateContractWallet w1 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h2 <- activateContractWallet w2 $ endpoints scholarship
    h2t <- activateContractWallet w2 VerifiedByToken.endpoints
    h2p <- activateContractWallet w2 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    h4t <- activateContractWallet w4 VerifiedByToken.endpoints
    h5p <- activateContractWallet w5 $ ScholarshipPool.poolEndpoints (ScholarshipPool.poolParamsToScholarship scholarship)

    callEndpoint @"donate" h5p 50_000_000 --First donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"donate" h5p 35_000_000 --Second Donation

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h2t (1,pkh1) -- Authority mints 1 acceptance token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (1,pkh1) -- School mints 1 school token and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"initScholarship" h1p pkh1 -- Recipient initiates personal scholarship

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h4t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 (ScholarshipDatum pkh1 0)  -- Recipient provides evidence of progress and recieves portion of scholarship.

    void $ Emulator.waitNSlots 3

    callEndpoint @"emergencyRefund" h2 (ScholarshipDatum pkh1 1) -- Authority asks for an emergency refund of the specific scholarship.

    void $ Emulator.waitNSlots 3

    callEndpoint @"refundPool" h2p pkh2 -- Authority asks for a refund of the scholarshippool.
    --The above currently causes an error because it tries to refund a utxo that was already spent in the previous steps. This is
    -- a wallet modelling error rather than a smart contract error.

    void $ Emulator.waitNSlots 3

