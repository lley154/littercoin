# Littercoin Open Source Repository
#### Table Of Contents
- [Problem Statement](#problem-statement)
- [The Solution](#the-solution)
- [The Application](#the-application)
  - [Application Design](#application-design)
  - [Adding Ada](#adding-ada)
  - [Minting Littercoin](#minting-littercoin)
  - [Minting Merchant Token](#minting-merchant-token)
  - [Burning Littercoin](#burning-littercoin)
- [Why Helios](#why-helios)
  - [Baseline Comparison](#baseline-comparison)
- [Getting Started](#getting-started)
  - [Nami Wallet Setup](#nami-wallet-setup)
  - [Demeter Run Setup](#demeter-run-setup)
- [Initialize The Smart Contract](#initialize-the-smart-contract)
  - [Create The Admin Keys And Address](#create-the-admin-keys-and-address)
  - [Send Ada To The Admin Address](#send-ada-to-the-admin-address)
  - [Determine The Admin UTXO](#determine-the-admin-utxo)
  - [Compile Smart Contract Code and Deploy](#compile-smart-contract-code-and-deploy)
  - [Threadtoken and Littercoin Initialization](#threadtoken-and-littercoin-initialization)
- [Starting The Web App](#starting-the-web-app)
  - [Update Environment variables and Start Next.js](#update-environment-variables-and-start-nextjs)
  - [Support/Issues/Community](#supportissuescommunity)   

## Problem Statement
Litter and plastic pollution are global problems. Crowdsourcing data can help fix this, but data collection tools, visualizations and incentives remain significantly underdeveloped
## The Solution
Littercoin is the first token rewarded for doing citizen science by simply walking around with a smart phone and start collecting information about your local environmental surroundings.
## The Application
#### Application Design
The following diagram depicts the typical user journey for littercoin.

![Littercoin User Journey](https://user-images.githubusercontent.com/7105016/226444708-c0768bfb-3fe4-4663-a02b-7f33e3579d05.png)


The high level design was used to create a model of the sequence of transactions, the datum state data and the inputs and outputs.

![Littercoin High Level Design](/images/littercoin_design.png)

##### Adding Ada
Any user with a Nami or Eternl wallet can go the web application and add Ada to the smart contract. They will received Littercoin Donation Rewards for every Ada they add to the smart contract.
##### Minting Littercoin
Only the user (after they have logged into the application) will be able to mint the amount of littercoins that they are due. The user will enter the address where to send the littercoin and mint them.   The littercoin application will check and confirm that the user is actually eligible, and will sign the transaction acordingly.
##### Minting Merchant Token
Only an admin who is logged into the littercoin application can mint merchant tokens.
##### Burning Littercoin
Only a wallet with a merchant token is able to burn littercoin and receive Ada.  The Merchant enters the total amount of littercoin they have in their wallet that they will burn.  The Smart Contract will then "burn" the littercoin and send them the amount of Ada corresponding to the current Ada:Littercoin price ratio.

## Why Helios
Helios is a fantastic alternative language for writing plutus smart contracts.   No nix, no cabal and no haskell yet a strongly typed, functional programming language!   The excellent documentation and well designed language and syntax is very intuitive and easy to learn.  Find out more info here: [https://github.com/Hyperion-BT/Helios](https://github.com/Hyperion-BT/Helios)
#### Baseline Comparison
Since I initially wrote the littercoin smart contract using Haskell Plutus Tx, I was able to compare a baseline, so here are the results.

![image](https://user-images.githubusercontent.com/7105016/209332105-bd872821-3120-46a7-89d0-d97aa0210f56.png)

Baseline results for Littercoin validator script:

Helios Plutus V2 baseline testing log: https://github.com/lley154/littercoin/blob/preprod-5.0/testing/baseline.log

Haskell Plutus V2 baseline testing log: https://github.com/lley154/littercoin/blob/baseline/testing/baseline.log

## Getting Started
To setup your own incentivized token economy, you can follow the steps below and use this as  a template.

#### Nami Wallet Setup
You will need to use a Cardano wallet and using the Nami wallet is a good wallet for new and experienced users alike.   Nami wallet is a Chrome browser extension wallet and can be found on the Chrome extension page here: 

https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo

For testing, it is recommended to setup 2 accounts.

Follow the instructions to create a new wallet and a seed phase for the 1st user we will call Owner.   Select the profile image to get to the account detail view, and from there you can create another account called User.

```
*** Important Step ***
Select the profile icon to get to access the Settings menu.   
Then select Network and select Preprod.
```

You will need some Ada, so you can go to the preprod test faucet page here:  

https://docs.cardano.org/cardano-testnet/tools/faucet

Select receive in the Nami wallet to get an address to send the funds to.   Use that address in the test faucet page and make sure you select preprod network.

Once you have some funds in the User account, you can proceed.

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
20. Enter Port Number as 3000
21. Select Expose
22. Now select the Open VSCode button (top right)
This will start a web based vscode instance.   You will need to authorize access when requested by vscode.  This is the way you will edit code and run commands in your workspace.

## Initialize The Smart Contract
To initialize the littercoin smart contract, we will need the admin UTXOs and the owner PKH.   This is used only once to run the init-tx.sh bash shell script.  

##### Create The Admin Keys And Address

1. Go to your Web VS Code in your browser
2. Select the hamburger menu (top left) and Terminal -> New Terminal
3. mkdir ~/.local/keys
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
13. ./cardano-cli address build --payment-verification-key-file ~/.local/keys/key.vkey --out-file ~/.local/keys/key.addr --testnet-magic 1
14. more ~/.local/keys/key.addr 

You will see something similar to the following:
```
abc@hallowed-birthday-3qoq5k-0:~/workspace/cardano-wallet-v2022-12-14-linux64$ more ~/.local/keys/key.addr
addr_test1v83ynr979e4xpjj28922y4t3sh84d0n08juy58am7jxmp4g6cgxr4
```

##### Send Ada To The Admin Address
You need to send 2 transactions to this address from your Nami wallet.

- Transaction #1 - 5 tAda
- Transaction #2 - 35 tAda

1. Open your Nami wallet and select Send
2. Copy and paste the admin address you created above as the receiving address
3. Sign and submit the transaction
4. You may need to wait 10 - 60 seconds for the transaction to complete

##### Determine The Admin UTXO
Now to see the UTXOs at your admin address, you can execute the following command

```
cardano-cli query utxo --address addr_test1v83ynr979e4xpjj28922y4t3sh84d0n08juy58am7jxmp4g6cgxr4 --cardano-mode --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d36e0a777ac7234a1dcf30a485dea1c68b81f1286f3c016e35ed5598652976e8     0        35000000 lovelace + TxOutDatumNone
ff5141fe2535284719b2261e78a97b2c3e7111210b6af56b5f6107a2938ee382     0        5000000 lovelace + TxOutDatumNone
```

Note: 1 Ada = 1,000,000 lovelace.


#### Compile Smart Contract Code and Deploy 
1. Open the Web VS Code editor and open the explorer tab on the left.  
2. Navigate to the src directory and open threadToken.hl file.
3. Find and replace the UTXO that you identified in finding the UTXO step above
```
// Define the UTXO to be spent
const TX_ID: ByteArray = #d36e0a777ac7234a1dcf30a485dea1c68b81f1286f3c016e35ed5598652976e8
```
4. Using the Web VS Code explorer, open merchToken.hl
5. Find and replace the PKH of the owner that was obtained above.
```
// Define the owner public key hash (PKH)
const OWNER_PKH: ByteArray = #b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682
```

Save the file
6. In a terminal window, go to the project root directory by typing 
```
cd ~/workspace/repo
```
7. Install deno testing a simple welcome typescript program
```
npx deno-bin run https://deno.land/std/examples/welcome.ts
```
Then execute the following command to compile the threadToken.hl file
```
npx deno-bin run --allow-read --allow-write ./src/deploy-init.js
thread token mph:  c644be7457a17fe5d6a2636bc30dc2acc2ef32cbecfa44daec10b58a
thread token name:  {"bytes": "54687265616420546f6b656e204c6974746572636f696e"}
merchant token mph:  10009086d699dfdd386ab1ddbfb6d6492228e039172f78af780f4686
merchant token name:  #4d65726368616e7420546f6b656e204c6974746572636f696e
```
8. Using the Web VS Code explorer, open lcMint.hl and rewardsToken.hl and update with the new thread token mph
9.Then execute the following command to compile the littercoin and rewards minting policies.
```
npx deno-bin run --allow-read --allow-write ./src/deploy-mint.js 
littercoin mph:  1f6b4c1fbe934b6e778e7086147b8c0a37b1cae31c2d118276c757cd
littercoin token name:  {"bytes": "4c6974746572636f696e"}
Donation rewards mph:  21b3fdc9da10189c19680788c1110d6e684c315614d7e725528827c1
Donation rewards token name:  {"bytes": "446f6e6174696f6e2052657761726473204c6974746572636f696e"
```
10.  Using the Web VS Code explorer, open the lcValidator.hl file and replace the threadtoken mph, the merchant mph and owner pkh with the output from above.  The other values remain the same so you don't need to update them unless you are changing the thread token name, littercoin token name and merchant token name.
```
// Define thread token value
const TT_MPH: ByteArray = #c644be7457a17fe5d6a2636bc30dc2acc2ef32cbecfa44daec10b58a
```
```
// Define the littercoin mph
const LC_MPH: ByteArray = #c644be7457a17fe5d6a2636bc30dc2acc2ef32cbecfa44daec10b58a
```
```
// Define the rewards mph
const REWARDS_MPH: ByteArray = #c644be7457a17fe5d6a2636bc30dc2acc2ef32cbecfa44daec10b58a
```
```
// Define the merchant mph
const MERCHANT_MPH: ByteArray = #10009086d699dfdd386ab1ddbfb6d6492228e039172f78af780f4686
```
```
// Define the pkh of the owner
const OWNER_PKH: ByteArray = #b68cf82d0cf89438a84bbf5506801e5a9372c3bcc7cfb7fb59b8d901 
```

11. Save the file and then run deno again
```
npx deno-bin run --allow-read --allow-write src/deploy-val.js 
littercoin validator hash:  1289f3bf1ffb1a1dd43f590dd641d85c3dc4f97bd60510216388a8d3
littercoin validator address:  addr_test1wrq55l5av8ff570h42cz88xhcl2fv0q5452hc44gdt8aldqp9hr70

Note: For production, the following flag need to be set false in helios.js
export var IS_TESTNET = false;
```
12.  Now copy the source and generated files in the src and deploy directories respectively
```
cp src/*.hl app/contracts/
cp deploy/* scripts/preprod/data
```

#### Threadtoken and Littercoin Initialization
1. Next we are going to initialize the smart contract
```
cd scripts
./init-tx.sh preprod
```
2. After waiting 10-60 seconds, you should be able to query the blockchain and see the threadtoken and littercoin locked at the smart contract.
```
cardano-cli query utxo --address addr_test1wz4t9har763a2wrv8qv2ltf400lkjpmxh8wa2ujyqc05fzgauxtdx --cardano-mode --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a5508a1bf3b5e33a0cd9363b5642e63a61f813d32ba87eba2d80766ce09f91c4     0        2000000 lovelace + 1 c644be7457a17fe5d6a2636bc30dc2acc2ef32cbecfa44daec10b58a.54687265616420546f6b656e204c6974746572636f696e + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataList [ScriptDataNumber 2000000,ScriptDataNumber 2])
a5508a1bf3b5e33a0cd9363b5642e63a61f813d32ba87eba2d80766ce09f91c4     1        30000000 lovelace + TxOutDatumNone
```
## Starting The Web App
#### Update Environment variables and Start Next.js
1. Using Web VS Code explorer, File->Open and in the popup window enter /config/.bashrc 
2. Add the following lines at the end of it, and use the correct values obtained from the above steps.
```
export NEXT_PUBLIC_BLOCKFROST_API_KEY="get-your-api-key-from-blockfrost"
export NEXT_PUBLIC_BLOCKFROST_API="https://cardano-preprod.blockfrost.io/api/v0"
export NEXT_PUBLIC_MIN_ADA=2000000
export NEXT_PUBLIC_MAX_TX_FEE=500000
export NEXT_PUBLIC_MIN_CHANGE_AMT=1000000
export NEXT_PUBLIC_NETWORK=preprod
export NEXT_PUBLIC_NETWORK_PARAMS_FILE="preprod.json"
export NEXT_PUBLIC_THREAD_TOKEN_NAME="54687265616420546f6b656e204c6974746572636f696e"
export NEXT_PUBLIC_LC_TOKEN_NAME="4c6974746572636f696e"
export NEXT_PUBLIC_OWNER_PKH="b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682"
export NEXT_PUBLIC_MERCH_TOKEN_NAME="4d65726368616e7420546f6b656e204c6974746572636f696e"
export NEXT_PUBLIC_REWARDS_TOKEN_NAME="446f6e6174696f6e2052657761726473204c6974746572636f696e"
export NEXT_PUBLIC_ROOT_KEY="c0be6e3ab769d0ba7e28f289bf7b0c29b98bc0978c4a79f4fe433ee17dda7e49d7e5744f0f78c69f86daff20bb05e4a48aeefd21f47e56432ef80add98d0e8371874d99d80a5264d9be5abbd4f51385047ffef5dfbb517c60b6c6923f6802aca"
```
3. Now read in the environment variables using the terminal window.
```
source ~/.bashrc
```
4. cd app
5. npm install
6. npm run dev
```
> app@0.1.0 dev
> next dev

ready - started server on 0.0.0.0:3000, url: http://localhost:3000
```
7. Now go to the proxy URL which was obtained when we exposed the port 3000 in the Demeter setup steps above.  The URL will look like the following:
```
https://3000-venomous-audience-7gg1mp.us1.demeter.run/
```

You should now have access to the basic web app for testing adding Ada, minting merchant tokens and minting and burning littercoin.

Enjoy!



## Support/Issues/Community

[Slack](https://join.slack.com/t/openlittermap/shared_invite/zt-fdctasud-mu~OBQKReRdC9Ai9KgGROw) is our main medium of communication and collaboration. Power-users, newcomers, developers, a community of over 400 members - we're all there. 
