amt=50000000
studentAddressFile=testStudent.addr
milestone=0
skeyFile=testDonor.skey
changeAddrFile=testDonor.addr
datumHashFile=datum.hash
txIn=?????


studentAddress=$(cat $studentAddressFile)
changeAddr=$(cat $changeAddrFile)

cabal exec writeDatum $datumHashFile $studentAddress 0

scriptAddress=$(cat scholarshipValidator.addr)

echo $amt
echo $scriptAddress 
echo $skeyFile 
echo $changeAddr
echo $datumHashFile
echo $txIn

./send.sh $amt $scriptAddress $skeyFile $changeAddr $datumHashFile $txIn