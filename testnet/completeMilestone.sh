#mustMint -1.
#mustValidateIn
#must put back (v - lovelaceValueOf (divide (sAmount scholarship) with milestone+1
#Note time is measured in slots here.

amount=100000000
milestone=$1
txMilestoneToken=$2
txClaim=$3
txInCollat=$4
recipientAddressFile=testStudent.addr
recipientAddress=$(cat $recipientAddressFile)
recipientSkey=testStudent.skey

scriptFile=scholarshipValidator.script
scriptAddressFile=scholarshipValidator.addr
cardano-cli address build --payment-script-file $scriptFile $TESTNET --out-file $scriptAddressFile
amountLeft=50000000
scriptAddress=$(cat $scriptAddressFile)
txOut="${scriptAddress}+${amountLeft}"
echo "txOut: $txOut"

mintAmt=-1
mintPolicyFile=milestonePolicy.script
pid=$(cardano-cli transaction policyid --script-file $mintPolicyFile)
tnHex=$(cabal exec tokenName -- $recipientAddress)
v="$mintAmt $pid.$tnHex"

datumFile=datumOld.hash
cabal exec writeScholDatum $datumFile $recipientAddress $milestone

nextMilestone=$(($milestone+1))
newDatumFile=datumNew.hash
cabal exec writeScholDatum $newDatumFile $recipientAddress $nextMilestone

redeemerFile=falseFalseRedeemer.json
cabal exec writeScholRedeemer $redeemerFile False False

cardano-cli transaction build \
    --babbage-era \
    $TESTNET \
    --change-address $recipientAddress \
    --invalid-before 10237027 \
    --invalid-hereafter 10238027 \
    --tx-in $txClaim \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in-collateral $txInCollat \
    --tx-in $txMilestoneToken \
    --mint "$v" \
    --mint-script-file milestonePolicy.script \
    --mint-redeemer-file unit.json \
    --tx-out $txOut \
    --tx-out-datum-hash-file $newDatumFile \
    --protocol-params-file protocol-parameters.json \
    --required-signer $recipientSkey \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    $TESTNET \
    --out-file tx.signed

cardano-cli transaction submit \
    $TESTNET \
    --tx-file tx.signed
