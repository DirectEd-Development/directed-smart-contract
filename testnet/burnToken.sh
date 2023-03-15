#!/bin/bash

#Here receiver is the person who is burning the token. (Generally assumed to be the recipient of the scholarship)
amt=-1
receiverAddrFile=testStudent.addr
receiverSkeyFile=testStudent.skey
mintingPolicyFile=$1
oref1=$2
oref2=$3

echo "receiver address file: $receiverAddrFile"
echo "amt: $amt"
echo "token oref: $oref1"
echo "collateral oref: $oref2"


ppFile=protocol-parameters.json
cardano-cli query protocol-parameters $TESTNET --out-file $ppFile

receiverAddr=$(cat $receiverAddrFile)

unitFile=unit.json
cabal exec writeUnit $unitFile

policyFile=acceptancePolicy.script

unsignedFile=tx.unsigned
signedFile=tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)

tnHex=$(cabal exec tokenName -- $receiverAddr)

v="$amt $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "verifier address: $verifierAddr"
echo "receiver address: $receiverAddr"


cardano-cli transaction build \
    $TESTNET \
    --babbage-era \
    --tx-in $oref1 \
    --tx-in-collateral $oref2 \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file $unitFile \
    --change-address $receiverAddr \
    --protocol-params-file $ppFile \
    --required-signer $receiverSkeyFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $receiverSkeyFile \
    $TESTNET \
    --out-file $signedFile

cardano-cli transaction submit \
    $TESTNET \
    --tx-file $signedFile