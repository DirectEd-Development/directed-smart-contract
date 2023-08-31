import {
  getAddressDetails,
  Address,
  AddressDetails,
} from "https://deno.land/x/lucid@0.10.6/mod.ts"

const kidusAddr: Address = "addr1qylzgv9pud8mq0lsasv22ttqqxn2mrns5xp5dnylrx5q7m7uhtl9sm6ftgufx6rqvrmag42lu7mc83dqpr8fklzsqmdqvcqxce"
const kidusDetails: AddressDetails = getAddressDetails(kidusAddr);
const kidusPKH: string = kidusDetails.paymentCredential.hash;

const simonAddr : Address = "addr_test1qr7ccpy6e6cy2jfs94pph7tw6vlnzr3h553k306wcrpwnhdc48wz039t2h5y636cvm3pyjtjwn53s2h8j22rak9xzvysxwqs6r"
const simonDetails: AddressDetails = getAddressDetails(simonAddr);
const simonPKH: string = simonDetails.paymentCredential.hash;

const edmundAddr : Address = "addr_test1qr7ccpy6e6cy2jfs94pph7tw6vlnzr3h553k306wcrpwnhdc48wz039t2h5y636cvm3pyjtjwn53s2h8j22rak9xzvysxwqs6r"
const edmundDetails: AddressDetails = getAddressDetails(edmundAddr);
const edmundPKH: string = edmundDetails.paymentCredential.hash;

console.log("Kidus PKH")
console.log(kidusPKH)
console.log("Simon PKH")
console.log(simonPKH)
console.log("Edmund PKH")
console.log(edmundPKH)

// Below are the commands required to test the full user flow of the contract. 
// Uncomment one at a time and run this file with: deno run -A lucid_functions.ts




