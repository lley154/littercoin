
import AddAda from '../components/AddAda';
import BurnLC from '../components/BurnLC';
import Head from 'next/head'
import LittercoinInfo from '../components/LittercoinInfo';
import LittercoinPool from '../components/LittercoinPool';
import MintLC from '../components/MintLC';
import MintMerchantToken from '../components/MintMerchantToken';
import MintOwnerToken from '../components/MintOwnerToken';
import type { NextPage } from 'next'
import styles from '../styles/Home.module.css'
import { useState, useEffect } from "react";

import WalletInfo from '../components/WalletInfo';
import {  
  TxHash,
  Unit,
  utf8ToHex,
  Blockfrost, 
  C, 
  Constr, 
  Data,
  Json, 
  Lucid,
  MintingPolicy,
  Network, 
  PolicyId,
  PlutusData, 
  } from "lucid-cardano"; // NPM
import { syncBuiltinESMExports } from 'module';

const Home: NextPage = () => {

  const [lcInfo, setLCInfo] = useState(
    {
        address : '',
        adaAmount: 0,
        lcAmount: 0,
        ratio: 0,
    }
  );
  const [whichWalletSelected, setWhichWalletSelected] = useState(undefined);
  const [walletIsEnabled, setWalletIsEnabled] = useState(false);
  const [API, setAPI] = useState<undefined | any>(undefined);
  const [wInfo, setWalletInfo] = useState({ balance : ''});
  const [tx, setTx] = useState({ txId : '' });


  useEffect(() => {
      const getContractInfo = async () => {
          const _info = await fetchLittercoinInfo();
          const _datum : PlutusData = _info?.datum as PlutusData;
          const _ada : number = Object.values(_datum)[1][0]; 
          const _lc : number = Object.values(_datum)[1][1];
          const _address : string = _info?.address as string;
          
          setLCInfo({
            ...lcInfo,
            address : _address,
            adaAmount : _ada,
            lcAmount : _lc,
          });
      }
      getContractInfo();
  }, []);  

  useEffect(() => {
    const checkWallet = async () => {
      
      setWalletIsEnabled(await checkIfWalletFound());
    }
    checkWallet();
  }, [whichWalletSelected]); 

  useEffect(() => {
    const enableSelectedWallet = async () => {
      if (walletIsEnabled) {
        const api = await enableWallet();
        setAPI(api);
      }
    }
    enableSelectedWallet();
  }, [walletIsEnabled]); 

  useEffect(() => {
    const updateWalletInfo = async () => {

        if (walletIsEnabled) {
            const _balance = await getBalance() as string;
            setWalletInfo({
              ...wInfo,
              balance : _balance
            });
        }           
    }
    updateWalletInfo();
  }, [API]);


  useEffect(() => {
    const updateLittercoinInfo = async () => {

      const sleep = (ms : number) => new Promise(r => setTimeout(r, ms));
      await sleep(30000);  // wait for the blockchain tx to propogate
      
      const _info = await fetchLittercoinInfo();
      const _datum : PlutusData = _info?.datum as PlutusData;
      const _ada : number = Object.values(_datum)[1][0]; 
      const _lc : number = Object.values(_datum)[1][1];
      const _address : string = _info?.address as string;
      
      setLCInfo({
        ...lcInfo,
        address : _address,
        adaAmount : _ada,
        lcAmount : _lc,
      });
    }
    updateLittercoinInfo();
  }, [tx]);



  const fetchLittercoinInfo = async () => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    const network = process.env.NEXT_PUBLIC_NETWORK as Network;

    const lucid = await Lucid.new(
      
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const threadToken = process.env.NEXT_PUBLIC_THREAD_TOKEN as string;

    console.log("lcValidatorScriptAddress", lcValidatorScriptAddress);
    console.log("threadToken", threadToken);
    
    const utxo = await lucid.utxosAt(lcValidatorScriptAddress);
    //console.log("utxo at script address", utxo);    

    // Iterate through the list of utxos
    for(let i=0; i<utxo.length; i++){

      // Find the utxo that has the thread token
      if (Object.keys(utxo[i].assets).includes(threadToken)) {

        // If found, then convert the CBOR represntation to PlutusData type
        if (utxo[i].datum != undefined) {
          const _datum : PlutusData = Data.from(utxo[i].datum as string);
          //console.log("datum found", _datum);
          return {datum: _datum, address: lcValidatorScriptAddress};
        }
      }
    }
  }

  // user selects what wallet to connect to
  const handleWalletSelect = (obj : any) => {
    const whichWalletSelected = obj.target.value
    setWhichWalletSelected(whichWalletSelected);
  }

  const checkIfWalletFound = async () => {
      
    let walletFound = false;

    const walletChoice = whichWalletSelected;
    if (walletChoice === "nami") {
        walletFound = !!window?.cardano?.nami;
    } else if (walletChoice === "eternl") {
        walletFound = !!window?.cardano?.eternl;
    } 
    return walletFound;
  }

  const checkIfWalletEnabled = async () => {

    let walletIsEnabled = false;
    try {
        const walletChoice = whichWalletSelected;
        if (walletChoice === "nami") {
            walletIsEnabled = await window.cardano.nami.isEnabled();

        } else if (walletChoice === "eternl") {
            walletIsEnabled = await window.cardano.eternl.isEnabled();
    
        } 
    } catch (err) {
        console.log('checkIfWalletEnabled error', err);
    }
    return walletIsEnabled;
  }


  const enableWallet = async () => {

    let walletAPI = undefined;
      try {
  
        const walletChoice = whichWalletSelected;
        if (walletChoice === "nami") {
            walletAPI = await window.cardano.nami.enable();
        } else if (walletChoice === "eternl") {
            walletAPI = await window.cardano.eternl.enable();
        } 
        return walletAPI 
    } catch (err) {
        console.log('enableWallet error', err);
    }
  }

  const getBalance = async () => {
    try {
        const balanceCBORHex = await API.getBalance();
        const balanceAmount = C.Value.from_bytes(Buffer.from(balanceCBORHex, "hex")).coin();
        const walletBalance : BigInt = BigInt(balanceAmount.to_str());
        return walletBalance.toLocaleString();
    } catch (err) {
        console.log('getBalance error', err);
    }
  }

  const mintLC = async (params : any) : Promise<TxHash> => {

    const address : string = params[0];
    const lcQty : number = params[1];
    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    var network;

    switch(process.env.NEXT_PUBLIC_NETWORK) { 
      case undefined: { 
         network = "Mainnet" as Network; 
         break; 
      } 
      case "Preprod": { 
         network = "Preprod" as Network;
         break; 
      } 
      default: { 
         network = "Mainnet" as Network;
         break; 
      } 
    } 

    const lucid = await Lucid.new(
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const ownerToken = process.env.NEXT_PUBLIC_OWNER_TOKEN as string;
    const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;

    lucid.selectWallet(API);

    const destPaymentCred = lucid.utils.getAddressDetails(address).paymentCredential;
    const destStakeCred = lucid.utils.getAddressDetails(address).stakeCredential;
    const returnPaymentCred = lucid.utils.getAddressDetails(await lucid.wallet.address()).paymentCredential;
    const returnStakeCred = lucid.utils.getAddressDetails(await lucid.wallet.address()).stakeCredential;

    console.log("destPaymentCred", destPaymentCred);
    console.log("destStakeCred", destStakeCred);
    console.log("returnPaymentCred", returnPaymentCred);
    console.log("returnStakeCred", returnStakeCred);

    const newDatum = Data.to(new Constr(0, [
      BigInt(Date.now()),                       // sequence number
      BigInt(lcQty),                            // amount of littercoin to mint
      utf8ToHex(destPaymentCred?.hash!),        // destination payment pkh
      utf8ToHex(destStakeCred?.hash!),          // desination stake pkh
      utf8ToHex(returnPaymentCred?.hash!),      // return payment pkh
      utf8ToHex(returnStakeCred?.hash!)]));     // return stake pkh

    const tx = await lucid
    .newTx()
    .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(minAda) , [ownerToken] : BigInt(1) })
    .complete();  

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
   } 

  const burnLC = async (lcQty : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    var network;

    switch(process.env.NEXT_PUBLIC_NETWORK) { 
      case undefined: { 
         network = "Mainnet" as Network; 
         break; 
      } 
      case "Preprod": { 
         network = "Preprod" as Network;
         break; 
      } 
      default: { 
         network = "Mainnet" as Network;
         break; 
      } 
    } 

    const lucid = await Lucid.new(
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const merchantToken = process.env.NEXT_PUBLIC_MERCHANT_TOKEN as string;
    const littercoinToken = process.env.NEXT_PUBLIC_LITTERCOIN_TOKEN as string;
    const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;

    lucid.selectWallet(API);

    const destPaymentCred = lucid.utils.getAddressDetails(await lucid.wallet.address()).paymentCredential;
    const destStakeCred = lucid.utils.getAddressDetails(await lucid.wallet.address()).stakeCredential;

    console.log("destPaymentCred", destPaymentCred);
    console.log("destStakeCred", destStakeCred);

    const newDatum = Data.to(new Constr(0, [
      BigInt(Date.now()),                       // sequence number
      BigInt(lcQty),                            // amount of littercoin to mint
      utf8ToHex(destPaymentCred?.hash!),        // destination payment pkh
      utf8ToHex(destStakeCred?.hash!),          // desination stake pkh
      utf8ToHex(""),                            // return payment pkh
      utf8ToHex("")]));                         // return stake pkh


    const tx = await lucid
    .newTx()
    .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(minAda) , [littercoinToken] : BigInt(lcQty) , [merchantToken] : BigInt(1) })
    .complete();  


    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  } 


  const mintMerchantToken = async (merchAddress : string) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;
    var network;

    switch(process.env.NEXT_PUBLIC_NETWORK) { 
      case undefined: { 
          network = "Mainnet" as Network; 
          break; 
      } 
      case "Preprod": { 
          network = "Preprod" as Network;
          break; 
      } 
      default: { 
          network = "Mainnet" as Network;
          break; 
      } 
    } 

    const lucid = await Lucid.new(
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    lucid.selectWallet(API);

    const { paymentCredential } = lucid.utils.getAddressDetails(
      await lucid.wallet.address(),
    );

    const mintingPolicy: MintingPolicy = lucid.utils.nativeScriptFromJson(
      {
        type: "all",
        scripts: [
          { type: "sig", keyHash: paymentCredential?.hash! },
        ],
      },
    );

    const policyId: PolicyId = lucid.utils.mintingPolicyToId(
      mintingPolicy,
    );
  
    const unit: Unit = policyId + utf8ToHex("Merchant Token Littercoin");

    const ownerAddress = await lucid.wallet.address();
    const ownerToken : string = process.env.NEXT_PUBLIC_OWNER_TOKEN as string;
    
    console.log("ownerToken", ownerToken);

    const tx = await lucid
      .newTx()
      .mintAssets({ [unit]: BigInt(1) })
      .validTo(Date.now() + 100000)
      .attachMintingPolicy(mintingPolicy)
      .payToAddress(merchAddress, { ["lovelace"] : BigInt(minAda), [unit]: BigInt(1) })
      .payToAddress(ownerAddress, { ["lovelace"] : BigInt(minAda), [ownerToken]: BigInt(1) })
      .complete();

    const signedTx = await tx.sign().complete();
  
    const txHash = await signedTx.submit();

    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  }   

  const mintOwnerToken = async (ownerAddress : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;
    var network;

    switch(process.env.NEXT_PUBLIC_NETWORK) { 
      case undefined: { 
          network = "Mainnet" as Network; 
          break; 
      } 
      case "Preprod": { 
          network = "Preprod" as Network;
          break; 
      } 
      default: { 
          network = "Mainnet" as Network;
          break; 
      } 
    } 

    const lucid = await Lucid.new(
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    lucid.selectWallet(API);

    const { paymentCredential } = lucid.utils.getAddressDetails(
      await lucid.wallet.address(),
    );

    const mintingPolicy: MintingPolicy = lucid.utils.nativeScriptFromJson(
      {
        type: "all",
        scripts: [
          { type: "sig", keyHash: paymentCredential?.hash! },
        ],
      },
    );

    const policyId: PolicyId = lucid.utils.mintingPolicyToId(
      mintingPolicy,
    );
  
    const unit: Unit = policyId + utf8ToHex("Owner Token Littercoin");

    const tx = await lucid
      .newTx()
      .mintAssets({ [unit]: BigInt(1) })
      .attachMintingPolicy(mintingPolicy)
      .payToAddress(ownerAddress, { ["lovelace"] : BigInt(minAda), [unit]: BigInt(1) })
      .complete();
  
    const signedTx = await tx.sign().complete();
  
    const txHash = await signedTx.submit();

    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  }  

  const addAda = async (adaQty : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    var network;

    switch(process.env.NEXT_PUBLIC_NETWORK) { 
      case undefined: { 
         network = "Mainnet" as Network; 
         break; 
      } 
      case "Preprod": { 
         network = "Preprod" as Network;
         break; 
      } 
      default: { 
         network = "Mainnet" as Network;
         break; 
      } 
    } 

    const lucid = await Lucid.new(
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    lucid.selectWallet(API);

    const mintingPolicy: MintingPolicy = lucid.utils.nativeScriptFromJson(
      {
        type: "all",
        scripts: [
          {
            type: "after",
            slot: 1001,
          },
        ],
      },
    );

    const policyId: PolicyId = lucid.utils.mintingPolicyToId(
      mintingPolicy,
    );
  
    const unit: Unit = policyId + utf8ToHex("Donation Littercoin");
    const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const lovelaceQty = adaQty * 1000000;
    const destPaymentCred = lucid.utils.getAddressDetails(await lucid.wallet.address()).paymentCredential;
    const destStakeCred = lucid.utils.getAddressDetails(await lucid.wallet.address()).stakeCredential;

    console.log("destPaymentCred", destPaymentCred);
    console.log("destStakeCred", destStakeCred);

    const newDatum = Data.to(new Constr(0, [
      BigInt(Date.now()),                       // sequence number
      BigInt(lovelaceQty ),                     // amount of Ada to add
      utf8ToHex(destPaymentCred?.hash!),        // destination payment pkh
      utf8ToHex(destStakeCred?.hash!),          // desination stake pkh
      utf8ToHex(""),                            // return payment pkh
      utf8ToHex("")]));                         // return stake pkh 

    const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: BigInt(1) })
    .attachMintingPolicy(mintingPolicy)
    .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(lovelaceQty), [unit]: BigInt(1) })
    .validFrom(Date.now() - 1000000)
    .complete();  

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  } 

  const littercoinPool = async (lcQty : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const blockfrost_url = process.env.NEXT_PUBLIC_BLOCKFROST_URL as string;
    var network;

    switch(process.env.NEXT_PUBLIC_NETWORK) { 
      case undefined: { 
         network = "Mainnet" as Network; 
         break; 
      } 
      case "Preprod": { 
         network = "Preprod" as Network;
         break; 
      } 
      default: { 
         network = "Mainnet" as Network;
         break; 
      } 
    } 

    const lucid = await Lucid.new(
      new Blockfrost(blockfrost_url, api_key),
      network,
    );

    lucid.selectWallet(API);
    
    const ownerAddress = await lucid.wallet.address();
    const ownerToken : string = process.env.NEXT_PUBLIC_OWNER_TOKEN as string;
    const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;

    const { paymentCredential } = lucid.utils.getAddressDetails(
      await lucid.wallet.address(),
    );

    const mintingPolicy: MintingPolicy = lucid.utils.nativeScriptFromJson(
      {
        type: "all",
        scripts: [
          { type: "sig", keyHash: paymentCredential?.hash! },
          {
            type: "before",
            slot: lucid.utils.unixTimeToSlot(Date.now() + 1000000),
          },
        ],
      },
    );
    
    const policyId: PolicyId = lucid.utils.mintingPolicyToId(
      mintingPolicy,
    );

    const unit: Unit = policyId + utf8ToHex("Littercoin");

    const newDatum = Data.to(new Constr(0, [
      BigInt(0),                  // sequence number
      BigInt(0),                  // amount of littercoin to mint
      utf8ToHex(""),              // destination payment pkh
      utf8ToHex(""),              // desination stake pkh
      utf8ToHex(""),              // return payment pkh
      utf8ToHex("")]));           // return stake pkh

    const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: BigInt(lcQty) })
    .validTo(Date.now() + 100000)
    .attachMintingPolicy(mintingPolicy)
    .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(minAda) , [unit] : BigInt(lcQty) })
    .payToAddress(ownerAddress, { ["lovelace"] : BigInt(minAda), [ownerToken]: BigInt(1) })
    .complete();  

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  } 


  return (
    <div className={styles.container}>
      <Head>
        <title>Littercoin</title>
        <meta name="description" content="Littercoin web tools page" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <h3 className={styles.title}>
          Littercoin {process.env.NEXT_PUBLIC_NETWORK}
        </h3>
        <div className={styles.border}>
          <h4>
            Littercoin Smart Contract
          </h4>
           <LittercoinInfo littercoinInfo={lcInfo}/>
        </div>
   
        <div className={styles.borderwallet}>
            <p>
              Connect to your wallet 
            </p>

            <p className={styles.borderwallet}>
              <input type="radio" id="eternl" name="wallet" value="eternl" onChange={handleWalletSelect}/>
                <label>Eternl</label>
            </p>
            <p className={styles.borderwallet}>
              <input type="radio" id="nami" name="wallet" value="nami" onChange={handleWalletSelect}/>
                <label>Nami</label>
            </p>
          </div>
            {walletIsEnabled && <div className={styles.border}><WalletInfo walletInfo={wInfo}/></div>}
            {tx.txId && <div className={styles.border}><b>Transaction Success!!!</b>
            <p>TxId &nbsp;&nbsp;<a href={"https://preprod.cexplorer.io/tx/" + tx.txId} target="_blank" rel="noopener noreferrer" >{tx.txId}</a></p>
            <p>Please wait until the transaction is confirmed on the blockchain and reload this page before doing another transaction</p>
          </div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><AddAda onAddAda={addAda}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintLC onMintLC={mintLC}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><BurnLC onBurnLC={burnLC}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintMerchantToken onMintMerchantToken={mintMerchantToken}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><LittercoinPool onLittercoinPool={littercoinPool}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintOwnerToken onMintOwnerToken={mintOwnerToken}/></div>}

      </main>

      <footer className={styles.footer}>

      </footer>
    </div>
  )
}

export default Home
