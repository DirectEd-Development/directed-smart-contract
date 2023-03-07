## Remove dependency on StateMachine module

# Scholarship.hs
Validator must be rewritten to not use StateMachine
Many supporting functions must also be rewritten
ScholarshipChooser may need to be included in off-chain code.
Off-chain code must be rewritten to not use runStep/runInitialize
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
    or by the emulator, so long as the transaction building step is NOT USING CONSTRAINTS. Is this possible? 

    Within Tx generation code in plutus is a fully specified Tx type. We can either use the standard code, but inspect this Tx type for clearer
    feedback, or we can attempt to fully specify it ourselves. 
    The second option would require figuring out how to manage: fees, collateral, signatures, 

# Notes
The __copy files are snapshots of what the files used to look like, for reference. 


# Changes Made
Changed redeemer to a single bool. 
Currently no deadline implemented. 

# Next up
Update dependencies to use plutus-apps. 

If this doesn't work, then do a shitty version of the oref chooser? 

Either:
    Update system so that we can use plutus V2, for instance 'DecoratedTxOut'.
    or Figure out a way to choose correct oref by inspecting chainIndexTxOut???