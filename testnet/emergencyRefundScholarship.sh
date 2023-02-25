
txClaim=$1
txInCollat=$2
expectedDatumFile=$3
refundAddrFile=testAuthority.addr
skeyFile=testAuthority.skey

redeemerFile=falseTrueRedeemer.json
refundAddr=$(cat $refundAddrFile)

cabal exec writeScholRedeemer $redeemerFile False True

cardano-cli transaction build \
    --babbage-era \
    $TESTNET \
    --change-address $refundAddr \
    --tx-in $txClaim \
    --tx-in-script-file scholarshipValidator.script \
    --tx-in-datum-file $expectedDatumFile \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in-collateral $txInCollat \
    --protocol-params-file protocol-parameters.json \
    --required-signer $skeyFile \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    $TESTNET \
    --out-file tx.signed

cardano-cli transaction submit \
    $TESTNET \
    --tx-file tx.signed
