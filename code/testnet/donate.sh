amt=$1
txIn=$2
toAddrFile=poolValidator.addr
skeyFile=testDonor.skey
changeAddrFile=testDonor.addr
datumFile=unit.json

cabal exec writeUnit "unit.json"

TESTNET="--testnet-magic 2"

toAddr=$(cat $toAddrFile)
changeAddr=$(cat $changeAddrFile)

echo "amt: $amt"
echo "toAddrFile: $toAddrFile"
echo "toAddr: $toAddr"
echo "skeyFile: $skeyFile"
echo "changeAddr: $changeAddr"
echo "datumFile: $datumFile"
echo "txIn: $txIn"

txOut="${toAddr}+${amt}"
echo "txOut: $txOut"

cardano-cli transaction build \
    --babbage-era \
    $TESTNET \
    --change-address $changeAddr \
    --tx-in $txIn \
    --tx-out $txOut \
    --tx-out-inline-datum-file $datumFile \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    $TESTNET \
    --out-file tx.signed

cardano-cli transaction submit \
    $TESTNET \
    --tx-file tx.signed