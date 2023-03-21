amt=100000000
studentPkhFile=testStudent.hash
milestone=0
skeyFile=testDonor.skey
changeAddrFile=testDonor.addr
datumFile=datum.json
TESTNET="--testnet-magic 2"
txIn=$1


studentPkh=$(cat $studentPkhFile)
changeAddr=$(cat $changeAddrFile)

echo $datumFile
echo $studentPkh
echo $milestone

cabal exec writeScholDatum $datumFile $studentPkh $milestone

scriptAddressFile=scholarshipValidator.addr
cardano-cli address build --payment-script-file scholarshipValidator.script $TESTNET --out-file $scriptAddressFile

echo $amt
echo $skeyFile 
echo $changeAddr
echo $datumFile
echo $txIn

./sendToScript.sh $amt $scriptAddressFile $skeyFile $changeAddr $datumFile $txIn