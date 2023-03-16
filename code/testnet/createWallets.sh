
USERS="Authority School CP Donor Student"

for USER in $USERS
do
    cardano-cli address key-gen --normal-key --verification-key-file test$USER.vkey --signing-key-file test$USER.skey
    cardano-cli address key-hash --payment-verification-key-file test$USER.vkey --out-file test$USER.hash
    cardano-cli address build --payment-verification-key-file test$USER.vkey --testnet-magic 2 --out-file test$USER.addr
done