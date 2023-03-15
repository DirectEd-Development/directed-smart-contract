## Remove dependency on StateMachine module

# Scholarship.hs
Validator must be rewritten to not use StateMachine
Many supporting functions must also be rewritten
Off-chain code must be rewritten in CLI or with Mesh/Lucid.
Simplify redeemer to only contain a single bool: refund. Currently allows D.E. to refund at any time, in case of mistakes. 

# ScholarshipPool.hs
Nothing explicitly needs to be rewritten.
However, structural review may be in order. 
In paricular the rules for calculating withdrawUpTo

# VerifiedByToken.hs
Reinvestigate the weirdness involving useOwnCurrencySymbol? 
If it doesn't work in the emulator, test in the CLI. 

# Utils.hs
Extra utils may need to be created in order to fully implement the contracts via the CLI. 

# Testing
Testing should either be done via the CLI (so that we can be sure of the validity of the tests),
    or via Mesh/Lucid. Alternatively using the Plutus Simple Model

# Notes


# Changes Made
Changed redeemer to a single bool. 
Currently no deadline implemented. 
Deleted 'app' folder containing executables. Will have to re-add later.
Changed Utilities: added code from Plutus.Scripts.Utils to allow conversions to CurrencySymbol, Validatorhash

# Next up
Get Cabal repl working. 

Consolidate my utils file and the new utils folder.

Incorporate old /app file so that I can generate scripts, addresses... 

Test using CLI.





Look into VerifiedByToken to see if we can use ownCurrencySymbol again? 

Investigate whether using Lucid/Mesh/PlutusSimpleModel is better for testing.
