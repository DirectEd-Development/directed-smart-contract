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
recipientPkhFile=testStudent.hash
recipientPkh=$(cat testStudent.hash)
recipientSkey=testStudent.skey

TESTNET="--testnet-magic 2"

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
tnHex=$(cabal exec tokenName -- $recipientPkh)
v="$mintAmt $pid.$tnHex"

datumFile=datumOld.json
cabal exec writeScholDatum $datumFile $recipientPkh $milestone

nextMilestone=$(($milestone+1))
newDatumFile=datumNew.json
cabal exec writeScholDatum $newDatumFile $recipientPkh $nextMilestone

redeemerFile=falseRedeemer.json
cabal exec writeScholRedeemer $redeemerFile False

cardano-cli transaction build \
    --babbage-era \
    $TESTNET \
    --change-address $recipientAddress \
    --tx-in $txClaim \
    --tx-in-script-file $scriptFile \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in-collateral $txInCollat \
    --tx-in $txMilestoneToken \
    --mint "$v" \
    --mint-script-file milestonePolicy.script \
    --mint-redeemer-file unit.json \
    --tx-out $txOut \
    --tx-out-inline-datum-file $newDatumFile \
    --protocol-params-file protocol-parameters.json \
    --required-signer $recipientSkey \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $recipientSkey \
    $TESTNET \
    --out-file tx.signed

cardano-cli transaction submit \
    $TESTNET \
    --tx-file tx.signed
