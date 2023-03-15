
authAddrFile=$1
schoolAddrFile=$2
coursePrAddrFile=$3
amount=100000000
milestones=2
#deadline=1708400000 #Equates to 20 Feb 2024
#passedDeadline=1668400000 #A deadline in the past, for testing purposes. 

authAddr=$(cat $authAddrFile)
schoolAddr=$(cat $schoolAddrFile)
coursePrAddr=$(cat $coursePrAddrFile)

cabal exec writeScripts $authAddr $schoolAddr $coursePrAddr $amount $milestones $deadline
