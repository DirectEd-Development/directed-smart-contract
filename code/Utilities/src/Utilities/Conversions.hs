module Utilities.Conversions
  ( Network (..)
  , validatorHash
  , validatorAddressBech32
  , posixTimeFromIso8601
  , posixTimeToIso8601
  , scriptCurrencySymbol
  ) where

import qualified Cardano.Api               as Script
import qualified Cardano.Api.Shelley       as Script
import           Cardano.Ledger.BaseTypes  (Network (..))
import           Cardano.Ledger.Credential (Credential (ScriptHashObj),
                                            StakeReference (StakeRefNull))
import qualified Data.Text                 as Text
import qualified Data.Time.Clock.POSIX     as Time
import qualified Data.Time.Format.ISO8601  as Time
import           Plutus.V2.Ledger.Api      as PV2
import           Utilities.Serialise       (validatorToScript)
import qualified PlutusTx.Builtins as Builtins
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as BSL
import Codec.Serialise (serialise)

toCardanoScriptScript :: PV2.Script -> Script.Script Script.PlutusScriptV2
toCardanoScriptScript =
    Script.PlutusScript
    Script.PlutusScriptV2
      . Script.PlutusScriptSerialised
      . SBS.toShort
      . BSL.toStrict
      . serialise

scriptHash :: PV2.Script -> PV2.ScriptHash
scriptHash =
    PV2.ScriptHash
    . Builtins.toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoScriptScript
    
-- | Hash a 'PV2.MintingPolicy' script.
mintingPolicyHash :: PV2.MintingPolicy -> PV2.MintingPolicyHash
mintingPolicyHash =
    PV2.MintingPolicyHash
  . PV2.getScriptHash
  . scriptHash
  . PV2.getMintingPolicy
  
{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'. Code taken from Plutus.Script.Utils.V2.Scripts
scriptCurrencySymbol :: PV2.MintingPolicy -> PV2.CurrencySymbol
scriptCurrencySymbol scrpt =
    let (PV2.MintingPolicyHash hsh) = mintingPolicyHash scrpt in PV2.CurrencySymbol hsh

validatorHash :: Validator -> Script.ScriptHash
validatorHash v = Script.hashScript $ Script.PlutusScript Script.PlutusScriptV2 $ validatorToScript v

validatorAddressBech32 :: Network -> Validator -> String
validatorAddressBech32 network v =
    Text.unpack $
    Script.serialiseToBech32 $
    Script.ShelleyAddress
      network
      (ScriptHashObj $ Script.toShelleyScriptHash $ validatorHash v)
      StakeRefNull

posixTimeFromIso8601 :: String -> Maybe POSIXTime
posixTimeFromIso8601 s = do
    t <- Time.formatParseM Time.iso8601Format s
    let seconds = Time.utcTimeToPOSIXSeconds t
        milliSeconds = round $ 1000 * seconds :: Integer
    return $ fromInteger milliSeconds

posixTimeToIso8601 :: POSIXTime -> String
posixTimeToIso8601 t = Time.formatShow Time.iso8601Format $ Time.posixSecondsToUTCTime $ fromRational $ toRational t / 1000

