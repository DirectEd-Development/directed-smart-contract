amt=$1
toAddr=$2
skeyFile=$3
changeAddr=$4
txIn=$5

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --change-address $changeAddr \
    --tx-in $txIn \
    --tx-out $toAddr" "$amt" lovelace" \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 2 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file tx.signed