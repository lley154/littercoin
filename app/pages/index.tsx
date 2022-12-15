
import AddAda from '../components/AddAda';
import BurnLC from '../components/BurnLC';
import Head from 'next/head'
import LittercoinInfo from '../components/LittercoinInfo';
import MintLC from '../components/MintLC';
import MintMerchantToken from '../components/MintMerchantToken';
import MintOwnerToken from '../components/MintOwnerToken';
import type { NextPage } from 'next'
import styles from '../styles/Home.module.css'
import { useState, useEffect } from "react";

import {
  Address, 
  Assets, 
  bytesToHex, 
  ByteArrayData,
  ConstrData, 
  Datum, 
  hexToBytes, 
  highlight,
  IntData, 
  TxId, 
  ListData, 
  MapData,
  MintingPolicyHash, 
  NetworkParams,
  Program, 
  PubKeyHash,
  Value, 
  TxOutput,
  TxRefInput, 
  TxWitnesses,
  UplcData, 
  Tx, 
  UTxO} from "@hyperionbt/helios";


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

let Buffer = require('buffer/').Buffer

const Home: NextPage = () => {

  const optimize = false;
  const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
  const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  //const netParamsUrl = process.env.NEXT_PUBLIC_NETWORK_PARAMS_URL as string;
  const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
  const threadTokenMPH = process.env.NEXT_PUBLIC_THREAD_TOKEN_MPH as string;
  const threadTokenName = process.env.NEXT_PUBLIC_THREAD_TOKEN_NAME as string;
  //const lcValRefTxId = process.env.NEXT_PUBLIC_THREAD_TOKEN as string;
  //const lcValRefTxIdx = process.env.NEXT_PUBLIC_THREAD_TOKEN as string;
  const ownerPkh = process.env.NEXT_PUBLIC_OWNER_PKH as string;

  
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
          const _datumObj = _info?.datum;
          const _ada : number = Object.values(_datumObj?.list[0]) as unknown as number;
          const _lc : number = Object.values(_datumObj?.list[1]) as unknown as number;
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
      const _datumObj = _info?.datum;
      const _ada : number = Object.values(_datumObj?.list[0]) as unknown as number;
      const _lc : number = Object.values(_datumObj?.list[1]) as unknown as number;
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



  // Get the reference script utxo and the utxo with the thread token
  const getUtxosBlockfrost = async (addr : string) => {

    //const blockfrostUrl : string = blockfrostAPI + "/addresses/" + addr + "/utxos/" + threadToken;
    const blockfrostUrl : string = blockfrostAPI + "/addresses/" + addr + "/utxos/?order=asc";

    var payload;
    let _res = await fetch(blockfrostUrl, {
      method: "GET",
      headers: {
        accept: "application/json",
        project_id: apiKey,
      },
    });

    if (_res?.status > 299) {
      payload = "[]";
    }

    payload = await _res.json();
    console.log("payload", payload);
    const threadToken = threadTokenMPH + threadTokenName;
    console.log("threadToken", threadToken);

    if (payload[1].amount[1].unit != threadToken) {
      throw console.error("thread token not found: ", payload[1].amount[1].unit);
    }

    const lovelaceAmount = payload[1].amount[0].quantity;
    const policyID = payload[1].amount[1].unit.substring(0, 56);
    const mph = MintingPolicyHash.fromHex(policyID);
    const token = hexToBytes(payload[1].amount[1].unit.substring(56));

    const value = new Value(BigInt(lovelaceAmount), new Assets([
        [mph, [
            [token, BigInt(1)]
        ]]
    ]));

    console.log("inlineDatum", payload[1].inline_datum);

    const response = await fetch('/api/lcValidator'); 
    const contractScript = await response.text();

    //console.log("data", contractScript);

    const compiledScript = Program.new(contractScript).compile(optimize);

    // Reference script utxo is the oldest so it is first,
    // next one is the utxo with the threadtoken
    return [new UTxO(
      TxId.fromHex(payload[0].tx_hash),
      BigInt(payload[0].output_index),
      new TxOutput(
        Address.fromBech32(addr),
        new Value(BigInt(payload[0].amount[0].quantity)),
        null,
        compiledScript
      )
    ), new UTxO(
      TxId.fromHex(payload[1].tx_hash),
      BigInt(payload[1].output_index),
      new TxOutput(
        Address.fromBech32(addr),
        value,
        Datum.inline(ListData.fromCbor(hexToBytes(payload[1].inline_datum)))
      )
    )];
  }


/*

  // Get the utxo with the thread token for a specific address
  const getUtxosBlockfrost = async (addr : string) => {

    const blockfrostUrl : string = blockfrostAPI + "/addresses/" + addr + "/utxos/" + threadToken;

    var payload;
    let _res = await fetch(blockfrostUrl, {
      method: "GET",
      headers: {
        accept: "application/json",
        project_id: apiKey,
      },
    });

    if (_res?.status > 299) {
      payload = "[]";
    }

    payload = await _res.json();
    console.log("payload", payload);

    const lovelaceAmount = payload[0].amount[0].quantity;
    const policyID = payload[0].amount[1].unit.substring(0, 56);
    const mph = MintingPolicyHash.fromHex(policyID);
    const token = hexToBytes(payload[0].amount[1].unit.substring(56));

    const value = new Value(lovelaceAmount, new Assets([
        [mph, [
            [token, BigInt(1)]
        ]]
    ]));

    console.log("inlineDatum", payload[0].inline_datum);

    return [new UTxO(
      TxId.fromHex(payload[0].tx_hash),
      BigInt(payload[0].output_index),
      new TxOutput(
        Address.fromBech32(addr),
        value,
        Datum.inline(ListData.fromCbor(hexToBytes(payload[0].inline_datum)))
      )
    )];

  }


  */

  const fetchLittercoinInfo = async () => {

    //console.log("lcValidatorScriptAddress", lcValidatorScriptAddress);
    //console.log("threadToken", threadToken);
    const utxo = await getUtxosBlockfrost(lcValidatorScriptAddress);
    console.log("utxo at script address", utxo);    

    if (utxo != undefined && utxo.length == 2) {

      if (!utxo[1].origOutput.datum.isInline()) {
          throw console.error("inline datum not found")
      }
      const _datumData = utxo[1].origOutput.datum.data;
      console.log("_datumData", _datumData);
      const _datumJson = _datumData.toSchemaJson();
      const _datumObj = JSON.parse(_datumJson);
      console.log("_datumObj", _datumObj);

      return {datum: _datumObj, address: lcValidatorScriptAddress};

    } else {
      console.log("fetchLittercoin: invalid number of utxos");
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
        const balanceAmountValue =  Value.fromCbor(hexToBytes(balanceCBORHex));
        const balanceAmount = balanceAmountValue.lovelace;
        const walletBalance : BigInt = BigInt(balanceAmount);
        return walletBalance.toLocaleString();
    } catch (err) {
        console.log('getBalance error: ', err);
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

    //const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const actionValScriptAddress = process.env.NEXT_PUBLIC_ACTION_VAL_ADDR as string;
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
    .payToContract(actionValScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(minAda) , [ownerToken] : BigInt(1) })
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

    //const lcValidatorScriptAddress = process.env.NEXT_PUBLIC_LC_VAL_ADDR as string;
    const actionValScriptAddress = process.env.NEXT_PUBLIC_ACTION_VAL_ADDR as string;
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
    .payToContract(actionValScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(minAda) , [littercoinToken] : BigInt(lcQty) , [merchantToken] : BigInt(1) })
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

    const _info = await fetchLittercoinInfo();
    const _datumObj = _info?.datum;
    const _ada : number = Object.values(_datumObj?.list[0]) as unknown as number;
    const _lc : number = Object.values(_datumObj?.list[1]) as unknown as number;
   
    const newAdaAmount : BigInt = BigInt(_ada) + BigInt(adaQty*1000000);

    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(BigInt(_lc));
    const newDatum = new ListData([newDatAda, newDatLC]);

    //const newDatum = Data.to(new Constr(0, [BigInt(newAdaAmount), BigInt(oldLCAmount)]));
    
    const valRedeemer = new ConstrData(
      0,
      []
    )

    //const validatorRedeemer = Data.to(new Constr(2, [BigInt(adaQty)]));
    
    const adaAmountVal = new Value(BigInt(adaQty*1000000));
    const cborUtxos = await API.getUtxos(bytesToHex(adaAmountVal.toCbor()));
    //const utxos = await API.getUtxos();

    let Utxos = [];

    for (const cborUtxo of cborUtxos) {
      const _utxo = UTxO.fromCbor(hexToBytes(cborUtxo));
      Utxos.push(_utxo);
    }

    console.log("Utxos", Utxos);


    const cborColatUtxo = await API.getCollateral();
    console.log("cborColatUtxo", cborColatUtxo[0] );
    const colatUtxo = UTxO.fromCbor(hexToBytes(cborColatUtxo[0]));

    const response = await fetch('/api/lcValidator'); 
    const contractScript = await response.text();

    //console.log("data", contractScript);

    const compiledScript = Program.new(contractScript).compile(optimize);
    const valAddr = Address.fromValidatorHash(true, compiledScript.validatorHash);

    console.log("valAddr", valAddr.toBech32());
    console.log("valHash", bytesToHex(compiledScript.hash()));


    const tx = new Tx();

    //const cborChangeAddr = await API.getChangeAddress();
    //console.log("cborChangeAddr", cborChangeAddr);
    //const changeAddr = Address.fromCbor(hexToBytes(cborChangeAddr));
    //console.log("changeAddr", cborChangeAddr);
    const changeAddr = Address.fromBech32("addr_test1vq7k907l7e59t52skm8e0ezsnmmc7h4xy30kg2klwc5n8rx5wscj60kfrsr64u3lf3sxnd7a375cscwe4t4jp39euxmqqchlvw");
    
    //const pkh = changeAddr.pubKeyHash;
    //console.log("change address pkh", pkh);

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }

    const valUtxos = await getUtxosBlockfrost(valAddr.toBech32());
    tx.addInput(valUtxos[1], valRedeemer);

    tx.addRefInput(
        valUtxos[0],
        compiledScript
    );

    // balance the tx and send back change
    //tx.addOutput(new TxOutput(changeAddr, changeVal));

    const newInlineDatum = Datum.inline(newDatum);
    const value = new Value(newAdaAmount.valueOf(), new Assets([
      [MintingPolicyHash.fromHex(threadTokenMPH), [
          [hexToBytes(threadTokenName), BigInt(1)]
      ]]
  ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(valAddr, value, newInlineDatum));

    //const changePkh : string = "003d62bfdff66855d150b6cf97e4509ef78f5ea6245f642adf7629338cd474312d3ec91c07aaf23f4c6069b7dd8fa98861d9aaeb20c4b9e1b6";
    //const pkh = PubKeyHash.fromHex(changePkh);
    //const pkh = changeAddr.pubKeyHash;
    //console.log("pkh", ownerPkh);
    //tx.addSigner(pkh);

    tx.addCollateral(colatUtxo);

    console.log("tx before final", tx.dump());

    const networkParams = new NetworkParams(
      await fetch("https://d1t0d7c2nekuk0.cloudfront.net/preprod.json")
          .then(response => response.json())
    )

    // send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);

    console.log("tx after final", tx.dump());

    console.log("Waiting for wallet signature...");

    //const pkws = await wallet.signTx(tx);
    const walletResp = await API.signTx(bytesToHex(tx.toCbor()), true)

    console.log("Verifying signature...");

    const signatures = TxWitnesses.fromCbor(hexToBytes(walletResp)).signatures

    tx.addSignatures(signatures)
    //tx.addSignatures(pkws);

    console.log("Submitting transaction...");

    //console.log(`submitted tx ${await network.submitTx(tx)}`);
    //const txHash = await network.submitTx(tx);
    
    const txHash = await API.submitTx(bytesToHex(tx.toCbor()));

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
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintOwnerToken onMintOwnerToken={mintOwnerToken}/></div>}

      </main>

      <footer className={styles.footer}>

      </footer>
    </div>
  )
}

export default Home
