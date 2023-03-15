amt=100000000
studentAddressFile=testStudent.addr
milestone=0
skeyFile=testDonor.skey
changeAddrFile=testDonor.addr
datumHashFile=datum.hash
txIn=$1


studentAddress=$(cat $studentAddressFile)
changeAddr=$(cat $changeAddrFile)

cabal exec writeScholDatum $datumHashFile $studentAddress $milestone

scriptAddressFile=scholarshipValidator.addr
cardano-cli address build --payment-script-file scholarshipValidator.script $TESTNET --out-file $scriptAddressFile

echo $amt
echo $skeyFile 
echo $changeAddr
echo $datumHashFile
echo $txIn

./sendToScript.sh $amt $scriptAddressFile $skeyFile $changeAddr $datumHashFile $txIn