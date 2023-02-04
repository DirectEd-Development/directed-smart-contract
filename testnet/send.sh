amt=$1
toAddr=$2
skeyFile=$3
changeAddr=$4
datumHashFile=$5
txIn=$6

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 2 \
    --change-address $changeAddr \
    --tx-in $txIn \
    --tx-out $toAddr" "$amt" lovelace" \
    --tx-out-datum-hash-file $datumHashFile \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 2 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file tx.signed