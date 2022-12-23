# Littercoin Open Source Repository
## Problem Statement
Litter and plastic pollution are global problems. Crowdsourcing data can help fix this, but data collection tools, visualisations and incentives remain significantly underdeveloped
## The Solution
Littercoin is the first token rewarded for doing citizen science by simply walking around with a smart phone and start collecting information about your local environmental surroundings.
## Getting Started
To setup your own incentized token economy, you can follow the steps below and use this as  a template.
#### Nami Wallet Setup
You will need to use a Cardano wallet and using the Nami wallet is a good wallet for new and experienced users alike.   Nami wallet is a Chrome browser extension wallet and can be found on the Chrome extension page here: 

https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo

For testing, it is recommended to setup 2 accounts.

Follow the instructions to create a new wallet and a seed phase for the 1st user we will call the Admin.   Select the profile image to get to the account detail view, and from there you can create another account called User.

You will need some Ada, so you can go to the preprod test faucet page here:  

https://docs.cardano.org/cardano-testnet/tools/faucet

Select receive in the Nami wallet to get an address to send the funds to.   Use that address in the test faucet page and make sure you select preprod network.

Once you have some funds in the Admin account, you can proceed.

#### Demeter Run Setup
Demeter Run is a fully hosted provider that creates workspaces where you can interact with the cardano node and build and launch web3 applications.  You will need to create a workspace (for free) for the steps below.
1. Go to https://demeter.run/ 
2. Sign in and create a New Project
3. Create or use an existing Organization
4. Select a cluster (US or Europe)
5. Select a plan Discover (free)
6. Select the Preprod Network
7. Enter a project name
8. Select Create Project
9. Select Open Project to go to the project console
10. On the Dashboard tab, select Setup a Dev Workspace
11. Make sure clone an existing repository is on
12. Use the following github URL: https://github.com/lley154/littercoin.git
13. Select your coding stack as Typescript
14. Select the small workspace size
15. Scroll to the bottom of the page and select "Create Workspace"
16. Wait for the workspace to be created
17. Select on the Exposed Ports tab
18. Select Exposed Port
19. Enter Port Name as Next.js
20. Enter Port Numer as 3000
21. Select Expose
22. Now select the Open VSCode button (top right)
This will start a web based vscode instance.   You will need to authorize access when requested by vscode.  This is the way you will edit code and run commands in your workspace.

#### Determine The Admin UTXO
To initialize the littercoin smart contract, we will need admin keys that is used one time to run the init-tx.sh bash shell script.  To determin the UTXO will require 3 main steps
1. Create the admin keys and address
2. Send funds to the admin address
3. Identify the UTXOs at the admin address

##### Create The Admin Keys And Address

1. Go to your Web VS Code in your browser
2. Select the hamburger menu (top left) and Terminal -> New Terminal
3. cd mkdir ~/.local/keys
2. cd ~/workspace
3. wget https://github.com/input-output-hk/cardano-wallet/releases/download/v2022-12-14/cardano-wallet-v2022-12-14-linux64.tar.gz
4. tar -xvzf cardano-wallet-v2022-12-14-linux64.tar.gz
5. cd cardano-wallet-v2022-12-14-linux64
7. ./cardano-address recovery-phrase generate --size 24 > ~/.local/keys/key.prv
8. ./cardano-address key from-recovery-phrase Shelley < ~/.local/keys/key.prv > ~/.local/keys/key.xprv
9. ./cardano-address key child 1852H/1815H/0H/0/0 < ~/.local/keys/key.xprv > ~/.local/keys/key.xsk
10. ./cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file ~/.local/keys/key.xsk --out-file ~/.local/keys/key.skey
11. ./cardano-cli key verification-key --signing-key-file ~/.local/keys/key.skey --verification-key-file ~/.local/keys/key.vkey
12. ./cardano-cli address key-hash --payment-verification-key-file ~/.local/keys/key.vkey --out-file ~/.local/keys/key.pkh
13. ./cardano-cli address build --mainnet --payment-verification-key-file ~/.local/keys/key.vkey --out-file ~/.local/keys/key.addr
14. more ~/.local/keys/key.addr 

You will see something similar to the following:
```
abc@hallowed-birthday-3qoq5k-0:~/workspace/cardano-wallet-v2022-12-14-linux64$ more ~/.local/keys/key.addr
addr1v83ynr979e4xpjj28922y4t3sh84d0n08juy58am7jxmp4g6cgxr4
```

##### Send Ada To The Admin Address
You need need to send 2 transactions to this address from your Nami wallet.

- Transaction #1 - 5 Ada
- Transaction #2 - 45 Ada

1. Open your Nami wallet and select Send
2. Copy and paste the admin address you created above as the receiving address
3. Sign and submit the transaction
4. You may need to wait 10 - 60 seconds for the transaction to complete

##### Determine The Admin UTXO
Now to see the UTXOs at your admin address, you can execute the following command

```
cardano-cli query utxo --address addr1v83ynr979e4xpjj28922y4t3sh84d0n08juy58am7jxmp4g6cgxr4 --cardano-mode --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d36e0a777ac7234a1dcf30a485dea1c68b81f1286f3c016e35ed5598652976e8     0        45000000 lovelace + TxOutDatumNone
ff5141fe2535284719b2261e78a97b2c3e7111210b6af56b5f6107a2938ee382     0        5000000 lovelace + TxOutDatumNone
```

Note: 1 Ada = 1,000,000 lovelace.


#### Determine The Owner PKH
The Owner is a business owner and does not have to the be same person as the admin and does not require a technical background.  The owner is the only one who can mint littercoin and mint merchant tokens.  We need to obtain the owner key so it can be hard coded into the smart contract.

A Cardano address is derived by the public key hash (PKH) that was created when you created your wallet.   The easiest way to get your pkh from the owner's wallet is to use the cardano-address command.   Open the Nami wallet and copy the receiving address to your clipboard.  Then, follow these steps:

1. Go to your Web VS Code in your browser
2. Select the hamburger menu (top left) and Terminal -> New Terminal
3. cd ~/workspace/cardano-wallet-v2022-12-14-linux64
4. Execute the following command to get your pkh
```
echo "paste-the-owner-address-from-nami-here" | ./cardano-address address inspect
```
Note: Please grant Web VS Code permission to access your clipboard

You will see the something like the following, and the value of the spending_key_hash without the quotes and is the pkh that we will need.
```
abc@hallowed-birthday-3qoq5k-0:~/workspace/repo/utils$ echo "addr_test1qzu6hnmgvageu2qyypy25yfqwg222tndt5eg3d6j68p8dqh30vtlz5gcmmrwxnquzf6g3d8are4elxmfpwpv83fm5ntqrew03n" | ./cardano-address address inspect
{
    "address_style": "Shelley",
    "address_type": 0,
    "network_tag": 0,
    "spending_key_hash": "**b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682**",
    "spending_key_hash_bech32": "addr_vkh1hx4u76r82x0zsppqfz4pzgrjzjjjum2ax2ytw5k3cfmgymje4ul",
    "stake_key_hash": "f17b17f15118dec6e34c1c127488b4fd1e6b9f9b690b82c3c53ba4d6",
    "stake_key_hash_bech32": "stake_vkh179a30u23rr0vdc6vrsf8fz95l50xh8umdy9c9s798wjdv8f4pge",
    "stake_reference": "by value"
}
```

#### Compile Smart Contract Code and Deploy 
1. Open the Web VS Code editor and open the explorer tab on the left.  
2. Navigate to the src directory and select the threadToken.hl file.
3. Find and replace the UTXO that you identified in finding the UTXO step above
```
const TX_ID: ByteArray = #8256ba4c73bf44e221b3f6fc759deafb309d1cec3c553d67c13537b248816027
```
4. Using the Web VS Code explorer, open merchToken.hl
5. Find and replace the PKH of the owner that was obatin above.
```
const OWNER_PKH: ByteArray = #b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682
```

7. Save the file
8. In a terminal window, go to the project root directory by typing 
```
cd ~/workspace/repo
```
7. Install deno testing a simple welcom typescript program
```
npx deno-bin run https://deno.land/std/examples/welcome.ts
```
8. The execute the following compand to compile the threadToken.hl file
```
npx deno-bin run --allow-read --allow-write ./src/deploy-init.js
thread token mph:  87e05280991949a0d85ad03a604bd81dbc4330cbd6ecbecb70f91ea8
thread token name:  #54687265616420546f6b656e204c6974746572636f696e
littercoin token name:  #4c6974746572636f696e
merchant token mph:  10009086d699dfdd386ab1ddbfb6d6492228e039172f78af780f4686
merchant token name:  #4d65726368616e7420546f6b656e204c6974746572636f696e
```
9. Using the Web VS Code explorer, select the lcValidator.hl file
10. Replace the threadtoken mph and the merchant PKH with the threadtoken mph and merchant PKH output from step 8 above.  The other values remain the same so you don't need to update them unless you are changing the thread token name, littercoin token name and merchant token name.
```
// Define thread token value
const TT_MPH: ByteArray = #87e05280991949a0d85ad03a604bd81dbc4330cbd6ecbecb70f91ea8
```
```
// Define the merchant token
const MERCHANT_MPH: ByteArray = #b63f671dcfbb0daaaf081f3b339243c7a533637d9755e5387a232a67
```
11. Save the file and then run deno again
```
npx deno-bin run --allow-read --allow-write src/deploy-val.js 
littercoin validator hash:  1289f3bf1ffb1a1dd43f590dd641d85c3dc4f97bd60510216388a8d3
```
12.  Now copy the source and generated files in the deploy directory
```
cp deploy/* scripts/preprod/data
cp src/* app/contracts/src
```

#### Threadtoken and Littercoin Initialization
1. Next we are going to intialize the smart contract
```
cd scripts
./init-tx.sh preprod
```
2. After waiting 10-60 seconds, you should be able to query the blockchain and see the threadtoken and littercoin locked at the smart contract.
```
cardano-cli query utxo --address addr_test1wrq55l5av8ff570h42cz88xhcl2fv0q5452hc44gdt8aldqp9hr70 --cardano-mode --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2ae13d9556351eefe2acd20bfe8c084847907140a6043a5b49849f21f469fe84     1        2000000 lovelace + 1000000000 1abf5744ba4c86034e50bcb23e2f04ef13cd7fb1b93b7d9fd2367835.4c6974746572636f696e + 1 1abf5744ba4c86034e50bcb23e2f04ef13cd7fb1b93b7d9fd2367835.54687265616420546f6b656e204c6974746572636f696e + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataList [ScriptDataNumber 0,ScriptDataNumber 0])
2ae13d9556351eefe2acd20bfe8c084847907140a6043a5b49849f21f469fe84     2        30000000 lovelace + TxOutDatumNone
```

#### Update Environment variables and Start Next.js
## The Application
#### Application Design

![Littercoin User Journey](/images/littercoin_user_journey.png)

![Littercoin High Level Design](/images/littercoin_design.png)


##### Adding Ada
##### Minting Littercoin
##### Burning Littercoin
## Why Helios
#### Performance Benchmarks





## The Littercoin Smart Contract 

The Littercoin smart contract can be tested directly on the hosted site [here](https://littercoin-smart-contract.vercel.app/)
or it can be downloaded and follow setup steps below.


## Clone the repository
```
git clone https://github.com/lley154/littercoin.git
```

## Setting up to test the littercoin smart contracts
```
export NEXT_PUBLIC_BLOCKFROST_API_KEY=>>>Your API Key Here <<<
sudo npm install --global yarn
cd littercoin/app
[littercoin/app]$ npm install
[littercoin/app]$ yarn dev
yarn run v1.22.19
$ next dev
ready - started server on 0.0.0.0:3000, url: http://localhost:3000
event - compiled client and server successfully in 1702 ms (173 modules)
```

Now you can access the preview testnet interfaces and testing the smart contracts by
going to the URL http://localhost:3000



## Support/Issues/Community

[Slack](https://join.slack.com/t/openlittermap/shared_invite/zt-fdctasud-mu~OBQKReRdC9Ai9KgGROw) is our main medium of communication and collaboration. Power-users, newcomers, developers, a community of over 400 members - we're all there. 
