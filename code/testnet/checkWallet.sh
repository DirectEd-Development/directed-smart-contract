addressFile=$1

address=$(cat $addressFile)

cardano-cli query utxo --address $address --testnet-magic 2
