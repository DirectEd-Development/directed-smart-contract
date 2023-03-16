amt=$1
toAddrFile=$2
skeyFile=$3
changeAddr=$4
datumHashFile=$5
txIn=$6

toAddr=$(cat $toAddrFile)

echo "amt: $amt"
echo "toAddrFile: $toAddrFile"
echo "toAddr: $toAddr"
echo "skeyFile: $skeyFile"
echo "changeAddr: $changeAddr"
echo "datumHashFile: $datumHashFile"
echo "txIn: $txIn"

txOut="${toAddr}+${amt}"
echo "txOut: $txOut"

cardano-cli transaction build \
    --babbage-era \
    $TESTNET \
    --change-address $changeAddr \
    --tx-in $txIn \
    --tx-out $txOut \
    --tx-out-datum-hash-file $datumHashFile \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    $TESTNET \
    --out-file tx.signed

cardano-cli transaction submit \
    $TESTNET \
    --tx-file tx.signed