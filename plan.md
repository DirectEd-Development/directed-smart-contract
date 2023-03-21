# VerifiedByToken.hs
Reinvestigate the weirdness involving useOwnCurrencySymbol? 
Test in CLI

# Notes
Seems students will require about 3Ada collateral. 

# Changes Made
Changed redeemer to a single bool. 
Currently no deadline implemented. 
Deleted 'app' folder containing executables. Will have to re-add later.
Changed Utilities: added code from Plutus.Scripts.Utils to allow conversions to CurrencySymbol, Validatorhash
Deleted my old Utils file.

# Next up
Look into what the error was? Looks like the collateral is returned normally and there is a single output to the change address of 2ish ada, as expected. So what goes wrong with low collateral???
    Seems that after calculating for ~1.4ada collateral being taken out, the CLI wants to be able to return the remaining collateral to the owners wallet. Since 2ada-1.4 leaves less than minAda, it doesn't like it. 

Test using CLI.

Write refund Pool script. 

Figure out how to combine change and return collateral outputs. Or how to withdraw the fees from the change output. So that we don't require a larger collateral input. 

Test if scholarshipPool having datum other than Unit causes an error!

Look into VerifiedByToken to see if we can use ownCurrencySymbol again? 

Investigate whether using Lucid/Mesh/PlutusSimpleModel is better for testing.

Think about who is paying the fees! (Isn't this covered by the tokens?)

Do we need to force/ask all donors to donate with inline-datum? Otherwise we can't access the datum? Or maybe it doesn't matter when we don't use datum in the script? 

Make the script faster? Use a different compiler like Plutarch/Plutonomicon/...?

