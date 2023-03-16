
authPkhFile=$1
schoolPkhFile=$2
coursePrPkhFile=$3
amount=100000000
milestones=2
deadline=1708400000 #Equates to 20 Feb 2024
#passedDeadline=1668400000 #A deadline in the past, for testing purposes. 

authPkh=$(cat $authPkhFile)
schoolPkh=$(cat $schoolPkhFile)
coursePrPkh=$(cat $coursePrPkhFile)

cabal exec writeScripts $authPkh $schoolPkh $coursePrPkh $amount $milestones $deadline

cardano-cli address build --payment-script-file scholarshipValidator.script --testnet-magic 2 --out-file scholarshipValidator.addr
cardano-cli address build --payment-script-file poolValidator.script --testnet-magic 2 --out-file poolValidator.addr