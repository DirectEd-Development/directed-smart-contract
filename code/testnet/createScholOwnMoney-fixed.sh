amt=100000000
studentPkhFile=testStudent.addr
milestone=0
skeyFile=testDonor.skey
changeAddrFile=testDonor.addr
datumHashFile=datum.hash
txIn=$1


studentPkh=$(cat $studentPkhFile)
changeAddr=$(cat $changeAddrFile)

cabal exec writeScholDatum $datumHashFile $studentPkh $milestone

scriptAddressFile=scholarshipValidator.addr
cardano-cli address build --payment-script-file scholarshipValidator.script $TESTNET --out-file $scriptAddressFile

echo $amt
echo $skeyFile 
echo $changeAddr
echo $datumHashFile
echo $txIn

./sendToScript.sh $amt $scriptAddressFile $skeyFile $changeAddr $datumHashFile $txIn