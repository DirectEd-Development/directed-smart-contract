#This works for exactly 1 claimed tx from the pool. 
amt=100000000
studentPkhFile=testStudent.hash
milestone=0
skeyFile=testStudent.skey
changeAddrFile=testStudent.addr
datumFile=datum.json
TESTNET="--testnet-magic 2"
txAcceptanceToken=$1
txSchoolToken=$2
txClaim=$3
txInCollat=$4
returnLovelace=$5

studentPkh=$(cat $studentPkhFile)
changeAddr=$(cat $changeAddrFile)

cabal exec writeScholDatum $datumFile $studentPkh $milestone

scholarshipAddressFile=scholarshipValidator.addr
cardano-cli address build --payment-script-file scholarshipValidator.script $TESTNET --out-file $scholarshipAddressFile
scholarshipAddr=$(cat $scholarshipAddressFile)

poolScriptFile=poolValidator.script
poolScriptAddressFile=poolValidator.addr
cardano-cli address build --payment-script-file $poolScriptFile $TESTNET --out-file $poolScriptAddressFile
poolAddr=$(cat $poolScriptAddressFile)


mintAmt=-1
tnHex=$(cabal exec tokenName -- $studentPkh)

acceptanceTokenPolicyFile=acceptancePolicy.script
acceptancePID=$(cardano-cli transaction policyid --script-file $acceptanceTokenPolicyFile)
acceptanceV="$mintAmt $acceptancePID.$tnHex"

schoolTokenPolicyFile=schoolPolicy.script
schoolPID=$(cardano-cli transaction policyid --script-file $schoolTokenPolicyFile)
schoolV="$mintAmt $schoolPID.$tnHex"


redeemerFile=studentPkhRedeemer.json
cabal exec writePoolRedeemer $redeemerFile $studentPkh

cabal exec writeUnit "unit.json"

ppFile=protocol-parameters.json
cardano-cli query protocol-parameters $TESTNET --out-file $ppFile

echo "amt: $amt"
echo "scholarshipAddressFile: $scholarshipAddressFile"
echo "scholarshipAddr: $scholarshipAddr"
echo "skeyFile: $skeyFile"
echo "changeAddr: $changeAddr"
echo "datumFile: $datumFile"
echo "txAcceptanceToken: $txAcceptanceToken"
echo "txSchoolToken: $txSchoolToken"
echo "txClaim: $txClaim"


txOutSchol="${scholarshipAddr}+${amt}"
echo "txOutSchol: $txOutSchol"

txOutPool="${poolAddr}+${returnLovelace}"
echo "txOutPool: $txOutPool"

cardano-cli transaction build \
    --babbage-era \
    $TESTNET \
    --protocol-params-file $ppFile \
    --change-address $changeAddr \
    --tx-in $txAcceptanceToken \
    --mint "$acceptanceV"+"$schoolV" \
    --mint-script-file acceptancePolicy.script \
    --mint-redeemer-file unit.json \
    --mint-script-file schoolPolicy.script \
    --mint-redeemer-file unit.json \
    --tx-in $txSchoolToken \
    --tx-in $txClaim \
    --tx-in-script-file $poolScriptFile \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in-collateral $txInCollat \
    --tx-out $txOutSchol \
    --tx-out-inline-datum-file $datumFile \
    --tx-out $txOutPool \
    --tx-out-inline-datum-file unit.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    $TESTNET \
    --out-file tx.signed

cardano-cli transaction submit \
    $TESTNET \
    --tx-file tx.signed