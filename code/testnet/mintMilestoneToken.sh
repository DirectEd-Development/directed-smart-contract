#!/bin/bash

amt=1
TESTNET="--testnet-magic 2"
verifierAddrFile=testCP.addr
verifierSkeyFile=testCP.skey
receiverAddrFile=testStudent.addr
receiverPkhFile=testStudent.hash
oref=$1

echo "receiver address file: $receiverAddrFile"
echo "amt: $amt"
echo "verifier address file: $verifierAddrFile"
echo "verifier signing key file: $verifierSkeyFile"
echo "oref: $oref"

ppFile=protocol-parameters.json
cardano-cli query protocol-parameters $TESTNET --out-file $ppFile

verifierAddr=$(cat $verifierAddrFile)
receiverAddr=$(cat $receiverAddrFile)
receiverPkh=$(cat $receiverPkhFile)

unitFile=unit.json
cabal exec writeUnit $unitFile

policyFile=milestonePolicy.script

unsignedFile=tx.unsigned
signedFile=tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)

tnHex=$(cabal exec tokenName -- $receiverPkh)

v="$amt $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "verifier address: $verifierAddr"
echo "receiver address: $receiverAddr"

#Is 1.5 ada correct?
cardano-cli transaction build \
    $TESTNET \
    --babbage-era \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$receiverAddr + 1500000 lovelace + $v" \
    --change-address $verifierAddr \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file $unitFile \
    --protocol-params-file $ppFile \
    --required-signer $verifierSkeyFile \
    --out-file $unsignedFile 

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $verifierSkeyFile \
    $TESTNET \
    --out-file $signedFile

cardano-cli transaction submit \
    $TESTNET \
    --tx-file $signedFile