{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TestContract where

import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import Data.Default               (Default (..))
import PlutusTx.Prelude
import Ledger.TimeSlot (slotToEndPOSIXTime)
import Control.Monad

import Contract
import qualified VerifiedByToken
import Prelude (IO)

test :: IO ()
test = runEmulatorTraceIO basicTrace

basicTrace :: EmulatorTrace ()
basicTrace = do 
    let (w1,w2,w3) = (knownWallet 1,knownWallet 2,knownWallet 3)
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    h3t <- activateContractWallet w3 VerifiedByToken.endpoints
    let pkh1      = mockWalletPaymentPubKeyHash w1
        pkh2      = mockWalletPaymentPubKeyHash w2 
        pkh3      = mockWalletPaymentPubKeyHash w3
        amount    = 50_000_000
        milestones= 2
        deadline  = slotToEndPOSIXTime def 5 -- Unused

        sp = ScholarshipParams
            { pRecipient        = pkh1
            , pAuthority        = pkh2 
            , pAuthoritySym     = VerifiedByToken.curSymbol pkh2 
            , pSchool           = pkh3
            , pSchoolSym        = VerifiedByToken.curSymbol pkh3 -- Unused
            , pCourseProvider   = pkh3
            , pCourseProviderSym= VerifiedByToken.curSymbol pkh3
            , pAmount           = amount
            , pMilestones       = milestones
            , pDeadline         = deadline
            }
        
    callEndpoint @"init" h2 sp -- Authority Initiates Scholarship for Recipient

    void $ Emulator.waitNSlots 3

    callEndpoint @"mint" h3t (2,pkh1) -- Courseprovider mints 2 milestone completion tokens and sends to recipient.

    void $ Emulator.waitNSlots 3

    callEndpoint @"progress" h1 sp  

    void $ Emulator.waitNSlots 3

    
    
 

