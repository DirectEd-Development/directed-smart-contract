toAddr=$1
skeyFile=$2
txIn=$3

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --change-address $toAddr \
    --tx-in $txIn \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file testnet/tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 2 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file tx.signed