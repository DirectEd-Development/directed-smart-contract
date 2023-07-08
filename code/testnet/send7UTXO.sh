toAddr=$1
skeyFile=$2
txIn1=$3
txIn2=$4
txIn3=$5
txIn4=$6
txIn5=$7
txIn6=$8
txIn7=$9

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --change-address $toAddr \
    --tx-in $txIn1 \
    --tx-in $txIn2 \
    --tx-in $txIn3 \
    --tx-in $txIn4 \
    --tx-in $txIn5 \
    --tx-in $txIn6 \
    --tx-in $txIn7 \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 2 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file tx.signed