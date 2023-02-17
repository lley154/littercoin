
import AddAda from '../components/AddAda';
import BurnLC from '../components/BurnLC';
import Head from 'next/head'
import LittercoinInfo from '../components/LittercoinInfo';
import MintLC from '../components/MintLC';
import MintMerchantToken from '../components/MintMerchantToken';
import type { NextPage } from 'next'
import styles from '../styles/Home.module.css'
import { useState, useEffect } from "react";
import WalletInfo from '../components/WalletInfo';

import {
  Address, 
  Assets, 
  bytesToHex, 
  ByteArrayData,
  Cip30Handle,
  Cip30Wallet,
  ConstrData, 
  Datum, 
  hexToBytes, 
  IntData, 
  TxId, 
  ListData, 
  NetworkParams,
  Program, 
  PubKeyHash,
  Value, 
  TxOutput,
  TxWitnesses,
  Tx, 
  UTxO,
  WalletHelper } from "@hyperionbt/helios";

  import path from 'path';
  import { promises as fs } from 'fs';

  declare global {
    interface Window {
        cardano:any;
    }
  }

  export async function getServerSideProps() {
  
    // set in env variables
    const optimize = false;
    const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
    const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;

    try {
      //Find the absolute path of the contracts directory
      const contractDirectory = path.join(process.cwd(), 'contracts');
      
      // Validator script
      const valFile = await fs.readFile(contractDirectory + '/lcValidator.hl', 'utf8');
      const valScript = valFile.toString();
      const compiledValScript = Program.new(valScript).compile(optimize);
      const valHash = compiledValScript.validatorHash; 
      const valAddr = Address.fromValidatorHash(valHash);
      
      // Littercoin minting script
      const mintFile = await fs.readFile(contractDirectory + '/lcMint.hl', 'utf8');
      const mintScript = mintFile.toString();
      
      // Thread token minting script
      const threadTokenFile = await fs.readFile(contractDirectory + '/threadToken.hl', 'utf8');
      const threadTokenScript = threadTokenFile.toString();
      
      // Merchant token minting script
      const merchTokenFile = await fs.readFile(contractDirectory + '/merchToken.hl', 'utf8');
      const merchTokenScript = merchTokenFile.toString();

      // Receipt token minting script
      const receiptTokenFile = await fs.readFile(contractDirectory + '/receiptToken.hl', 'utf8');
      const receiptTokenScript = receiptTokenFile.toString();


      const blockfrostUrl : string = blockfrostAPI + "/addresses/" + valAddr.toBech32() + "/utxos/?order=asc";

      var payload;
      let resp = await fetch(blockfrostUrl, {
        method: "GET",
        headers: {
          accept: "application/json",
          project_id: apiKey,
        },
      });
  
      if (resp?.status > 299) {
        throw console.error("Blockfrost API error", resp)
      }
      payload = await resp.json();

      // Find the reference utxo with the correct validator hash
      if (payload) {
        for (var utxo of payload) {
          if (utxo.reference_script_hash === valHash.hex) {
            const lcVal = {
              lcValScript: valScript,
              lcValAdaAmt: utxo.amount[0].quantity, 
              lcRefTxId: utxo.tx_hash,
              lcRefTxIdx: utxo.output_index,
              lcMintScript: mintScript,
              ttMintScript: threadTokenScript,
              mtMintScript: merchTokenScript,
              recMintScript: receiptTokenScript
            }
            return { props: lcVal }
          }
        }
      }
    } catch (err) {
      console.log('getServerSideProps', err);
    } 
    // No valid reference utxo found
    return { props: {} };
  }


const Home: NextPage = (props: any) => {

  const optimize = false;

  // Littercoin validator script
  const lcValScript = props.lcValScript as string;
  const compiledValScript = Program.new(lcValScript).compile(optimize);
  const lcValHash = compiledValScript.validatorHash; 
  const lcValAddr = Address.fromValidatorHash(lcValHash);

  // Littercoin minting script
  const lcMintScript = props.lcMintScript as string;
  const compiledLCMintScript = Program.new(lcMintScript).compile(optimize);
  const lcTokenMPH = compiledLCMintScript.mintingPolicyHash;

  // Thread token minting script
  const ttMintScript = props.ttMintScript as string;
  const compiledTTMintScript = Program.new(ttMintScript).compile(optimize);
  const threadTokenMPH = compiledTTMintScript.mintingPolicyHash;

  // Merchant token minting script
  const mtMintScript = props.mtMintScript as string;
  const compiledMerchMintScript = Program.new(mtMintScript).compile(optimize);
  const merchTokenMPH = compiledMerchMintScript.mintingPolicyHash;

  // Receipt token minting script
  const recMintScript = props.recMintScript as string;
  const compiledRecMintScript = Program.new(recMintScript).compile(optimize);
  const recTokenMPH = compiledRecMintScript.mintingPolicyHash;
  
  // Littercoin reference UTXO
  const lcValAdaAmt = props.lcValAdaAmt as string;
  const lcValRefTxId = props.lcRefTxId as string;
  const lcValRefTxIdx = props.lcRefTxIdx as string;

  const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
  const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  const threadTokenName = process.env.NEXT_PUBLIC_THREAD_TOKEN_NAME as string;
  const lcTokenName = process.env.NEXT_PUBLIC_LC_TOKEN_NAME as string;
  const merchTokenName = process.env.NEXT_PUBLIC_MERCH_TOKEN_NAME as string;
  const recTokenName = process.env.NEXT_PUBLIC_REC_TOKEN_NAME as string;
  const networkParamsUrl = process.env.NEXT_PUBLIC_NETWORK_PARAMS_URL as string;
  const ownerPkh = process.env.NEXT_PUBLIC_OWNER_PKH as string;
  const minAda = BigInt(process.env.NEXT_PUBLIC_MIN_ADA as string);
  const maxTxFee = BigInt(process.env.NEXT_PUBLIC_MAX_TX_FEE as string);
  const minChangeAmt = BigInt(process.env.NEXT_PUBLIC_MIN_CHANGE_AMT as string);
  //const serviceFee = BigInt(process.env.NEXT_PUBLIC_MAX_SERVICE_FEE as string);

  const [lcInfo, setLCInfo] = useState(
    {
        address : '',
        adaAmount: 0,
        lcAmount: 0,
        ratio: 0,
    }
  );
  const [valUtxo, setUTXO] = useState<undefined | any>(undefined);
  const [valRefUtxo, setRefUTXO] = useState<undefined | any>(undefined);
  const [networkParams, setNetworkParams] = useState<undefined | any>(undefined);
  const [walletAPI, setWalletAPI] = useState<undefined | any>(undefined);
  const [tx, setTx] = useState({ txId : '' });
  const [walletInfo, setWalletInfo] = useState({ balance : ''});
  const [walletIsEnabled, setWalletIsEnabled] = useState(false);
  const [whichWalletSelected, setWhichWalletSelected] = useState(undefined);

  useEffect(() => {
      const getContractInfo = async () => {
          const info = await fetchLittercoinInfo();
          const datObj = info?.datum;
          const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
          const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
          const datAddr : string = info?.address as unknown as string;
          
          setLCInfo({
            ...lcInfo,
            address : datAddr,
            adaAmount : datAda,
            lcAmount : datLC,
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
        setWalletAPI(api);
      }
    }
    enableSelectedWallet();
  }, [walletIsEnabled]); 

  useEffect(() => {
    const updateWalletInfo = async () => {

        if (walletIsEnabled) {
            const _balance = await getBalance() as string;
            setWalletInfo({
              ...walletInfo,
              balance : _balance
            });
            await setLCValRefUtxo();
        }           
    }
    updateWalletInfo();
  }, [walletAPI]);

  useEffect(() => {
    const updateLittercoinInfo = async () => {

      const sleep = (ms : number) => new Promise(r => setTimeout(r, ms));
      await sleep(30000);  // wait for the blockchain tx to propogate
      
      const info = await fetchLittercoinInfo();
      const datObj = info?.datum;
      const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
      const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
      const datAddr : string = info?.address as unknown as string;
      
      setLCInfo({
        ...lcInfo,
        address : datAddr,
        adaAmount : datAda,
        lcAmount : datLC,
      });
    }
    updateLittercoinInfo();
  }, [tx]);

  
  // Get the utxo with the thread token at the LC validator address
  const getTTUtxo = async () => {

    const blockfrostUrl : string = blockfrostAPI + "/addresses/" + lcValAddr.toBech32() + "/utxos/" + threadTokenMPH.hex + threadTokenName;
    
    var payload;
    let resp = await fetch(blockfrostUrl, {
      method: "GET",
      headers: {
        accept: "application/json",
        project_id: apiKey,
      },
    });

    if (resp?.status > 299) {
      throw console.error(resp);
    }

    payload = await resp.json();

    if (payload.length == 0) {
      throw console.error("thread token not found")
    }
    const lovelaceAmount = payload[0].amount[0].quantity;
    const token = hexToBytes(threadTokenName);
    const value = new Value(BigInt(lovelaceAmount), new Assets([
        [threadTokenMPH, [
            [token, BigInt(1)]
        ]]
    ]));

    const ttUtxo = new UTxO(
      TxId.fromHex(payload[0].tx_hash),
      BigInt(payload[0].output_index),
      new TxOutput(
        lcValAddr,
        value,
        Datum.inline(ListData.fromCbor(hexToBytes(payload[0].inline_datum)))
      )
    );
    setUTXO(ttUtxo);
    return ttUtxo;
  }

  const setLCValRefUtxo = async () => {

    const valRefUTXO = new UTxO (
      TxId.fromHex(lcValRefTxId),
      BigInt(lcValRefTxIdx),
      new TxOutput(
        lcValAddr,
        new Value(BigInt(lcValAdaAmt)),
        null,
        compiledValScript
      )
    )
    setRefUTXO(valRefUTXO);

    const networkParams = new NetworkParams(
      await fetch(networkParamsUrl)
          .then(response => response.json())
    );
    setNetworkParams(networkParams);
  }

  const fetchLittercoinInfo = async () => {

    const utxo = await getTTUtxo();

    if (utxo != undefined) {

      if (!utxo.origOutput.datum.isInline()) {
          throw console.error("inline datum not found")
      }
      const datData = utxo.origOutput.datum.data;
      const datJson = datData.toSchemaJson();
      const datObj = JSON.parse(datJson);

      return {datum: datObj, address: lcValAddr.toBech32()};

    } else {
      console.log("fetchLittercoin: thread token not found");
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

      try {
        const walletChoice = whichWalletSelected;
        if (walletChoice === "nami") {
            const handle: Cip30Handle = await window.cardano.nami.enable();
            const walletAPI = new Cip30Wallet(handle);
            return walletAPI;
          } else if (walletChoice === "eternl") {
            const handle: Cip30Handle = await window.cardano.eternl.enable();
            const walletAPI = new Cip30Wallet(handle);
            return walletAPI;
          } 
    } catch (err) {
        console.log('enableWallet error', err);
    }
  }

  const getBalance = async () => {
    try {
        const walletHelper = new WalletHelper(walletAPI);
        const balanceAmountValue  = await walletHelper.calcBalance();
        const balanceAmount = balanceAmountValue.lovelace;
        const walletBalance : BigInt = BigInt(balanceAmount);
        return walletBalance.toLocaleString();
    } catch (err) {
        console.log('getBalance error: ', err);
    }
  }

  const submitTx = async (tx: Tx) : Promise<string> => {
    const data = new Uint8Array(tx.toCbor());
    const url = blockfrostAPI + "/tx/submit";

    return new Promise((resolve, reject) => {
        const req = new XMLHttpRequest();
        req.onload = (_e) => {
            if (req.status == 200) {
                resolve(req.responseText);
            } else {
                reject(new Error(req.responseText));
            }
        }

        req.onerror = (e) => {
            reject(e);
        }

        req.open("POST", url, false);

        req.setRequestHeader("content-type", "application/cbor");
        req.setRequestHeader("project_id", apiKey);
        
        req.send(data);
    });   
  }

  const mintLC = async (params : any) => {

    const address = params[0];
    const lcQty = params[1];
    const newLCAmount : BigInt = BigInt(lcInfo.lcAmount) + BigInt(lcQty);
    const newDatAda = new IntData(BigInt(lcInfo.adaAmount));
    const newDatLC = new IntData(newLCAmount.valueOf());
    const newDatum = new ListData([newDatAda, newDatLC]);
    const minUTXOVal = new Value(minAda + maxTxFee + minChangeAmt);

    // Get wallet UTXOs
    const walletHelper = new WalletHelper(walletAPI);
    const utxos = await walletHelper.pickUtxos(minUTXOVal);
  
    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();

    // Start building the transaction
    const tx = new Tx();

    // Add the UTXO as inputs
    tx.addInputs(utxos[0]);

    // Construct the mint littercoin validator redeemer
    const valRedeemer = new ConstrData(1,[])
    tx.addInput(valUtxo, valRedeemer);

    tx.addRefInput(
        valRefUtxo,
        compiledValScript
    );

    const newInlineDatum = Datum.inline(newDatum);
    const outputValue = new Value(BigInt(lcInfo.adaAmount), new Assets([
      [threadTokenMPH, [
          [hexToBytes(threadTokenName), BigInt(1)]
      ]]
    ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(lcValAddr, outputValue, newInlineDatum));

    // Add the script as a witness to the transaction
    tx.attachScript(compiledLCMintScript);

    // Construct a mint littecoin minting redeemer
    const mintRedeemer = new ConstrData(0, [new ByteArrayData(lcValHash.bytes)])
    const tokens: [number[], bigint][] = [[hexToBytes(lcTokenName), BigInt(lcQty)]];

    tx.mintTokens(
      lcTokenMPH,
      tokens,
      mintRedeemer
    )

    tx.addOutput(new TxOutput(
      Address.fromBech32(address),
      new Value(minAda, new Assets([[lcTokenMPH, tokens]]))
    ));

    tx.addCollateral(colatUtxo);

    tx.addSigner(PubKeyHash.fromHex(ownerPkh));

    console.log("tx before final", tx.dump());

    // Send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    console.log("Submitting transaction...");
    const txHash = await walletAPI.submitTx(tx);
    console.log("txHash", txHash.hex);
    setTx({ txId: txHash.hex });
   } 

  const burnLC = async (lcQty : any) => {

    const lcQtyAbs: number = Math.abs(Number(lcQty));
    const newLCAmount : BigInt = BigInt(lcInfo.lcAmount) - BigInt(lcQtyAbs);
    const ratio : number = lcInfo.adaAmount / lcInfo.lcAmount;
    const withdrawAda : number = Number(ratio) * lcQtyAbs;
    const adaDiff: number = lcInfo.adaAmount - withdrawAda;
    var newAdaAmount: BigInt;
    if (adaDiff >= minAda) {
      newAdaAmount = BigInt(lcInfo.adaAmount) - BigInt(withdrawAda);
    } else {
      //newAdaAmount = BigInt(lcInfo.adaAmount) - BigInt(withdrawAda);
      throw console.error("Insufficient funds in Littercoin contract");
    }

    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(newLCAmount.valueOf());
    const newDatum = new ListData([newDatAda, newDatLC]);
    
    // Construct the Ada value that we need to spend from the wallet
    const minUTXOVal = new Value(minAda + maxTxFee + minChangeAmt);

    // Construct the littercoin token value to be spent from the wallet
    const lcTokens: [number[], bigint][] = [[hexToBytes(lcTokenName), BigInt(lcQty)]];
    const lcVal: Value = new Value(BigInt(minAda), new Assets([[lcTokenMPH, lcTokens]]));

    // Construct the merchant token to be spent from the wallet
    const merchTokens: [number[], bigint][] = [[hexToBytes(merchTokenName), BigInt(1)]];
    const merchVal: Value = new Value(BigInt(minAda), new Assets([[merchTokenMPH, merchTokens]]));

    // Get wallet UTXOs
    const walletHelper = new WalletHelper(walletAPI);
    const utxos = await walletHelper.pickUtxos(minUTXOVal.add(lcVal).add(merchVal));
    console.log("utxos", utxos);
  
    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();

    // Start building the transaction
    const tx = new Tx();

    tx.addInputs(utxos[0]);

    // Construct the burn littercoin validator redeemer
    const valRedeemer = new ConstrData(2, [new ByteArrayData(changeAddr.pubKeyHash.bytes)])
    tx.addInput(valUtxo, valRedeemer);

    tx.addRefInput(
        valRefUtxo,
        compiledValScript
    );

    const newInlineDatum = Datum.inline(newDatum);
    const outputValue = new Value(newAdaAmount.valueOf(), new Assets([
      [threadTokenMPH, [
          [hexToBytes(threadTokenName), BigInt(1)]
      ]]
    ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(lcValAddr, outputValue, newInlineDatum));

    // Add the script as a witness to the transaction
    tx.attachScript(compiledLCMintScript);

    // Construct a burn littecoin minting redeemer
    const mintRedeemer = new ConstrData(1, [new ByteArrayData(lcValHash.bytes)]);
    
    // Construct the littercoin token value to be burned
    const lcBurnTokens: [number[], bigint][] = [[hexToBytes(lcTokenName), (BigInt(lcQty) * BigInt(-1))]];

    tx.mintTokens(
      lcTokenMPH,
      lcBurnTokens,
      mintRedeemer
    )
    
    // Construct the merchant token value to be returned
    const merchAdaVal: Value = new Value(BigInt(minAda), new Assets([[merchTokenMPH, merchTokens]]));

    tx.addOutput(new TxOutput(
      changeAddr,
      merchAdaVal
    ));

    // Construct the ada withdraw amount
    const withdrawAdaVal: Value = new Value(BigInt(withdrawAda));

    tx.addOutput(new TxOutput(
      changeAddr,
      withdrawAdaVal
    ));

    tx.addCollateral(colatUtxo);

    console.log("tx before final", tx.dump());

    // Send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    console.log("Submitting transaction...");
    const txHash = await walletAPI.submitTx(tx);
    console.log("txHash", txHash.hex);
    setTx({ txId: txHash.hex });
   } 


  const mintMerchantToken = async (merchAddress : string) => {

    const minUTXOVal = new Value(minAda + maxTxFee + minChangeAmt);

    // Get wallet UTXOs
    const walletHelper = new WalletHelper(walletAPI);
    const utxos = await walletHelper.pickUtxos(minUTXOVal);
  
    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();

    // Start building the transaction
    const tx = new Tx();

    // Add the UTXO as inputs
    tx.addInputs(utxos[0]);

    // Add the script as a witness to the transaction
    tx.attachScript(compiledMerchMintScript);

    // Create an empty Redeemer because we must always send a Redeemer with
    // a plutus script transaction even if we don't actually use it.
    const mintRedeemer = new ConstrData(0, []);

    const tokens: [number[], bigint][] = [[hexToBytes(merchTokenName), BigInt(1)]];

    tx.mintTokens(
      merchTokenMPH,
      tokens,
      mintRedeemer
    )

    tx.addOutput(new TxOutput(
      Address.fromBech32(merchAddress),
      new Value(minAda, new Assets([[merchTokenMPH, tokens]]))
    ));

    tx.addCollateral(colatUtxo);

    tx.addSigner(PubKeyHash.fromHex(ownerPkh));

    console.log("tx before final", tx.dump());

    // Send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    console.log("Submitting transaction...");
    const txHash = await walletAPI.submitTx(tx);
    console.log("txHash", txHash.hex);
    setTx({ txId: txHash.hex });
  }   

  const addAda = async (adaQty : any) => {

    const lovelaceQty =  Number(adaQty) * 1000000;
    if (lovelaceQty < minAda) {
      throw console.error("Minimum donation is 2 Ada");
    }
    const newAdaAmount : BigInt = BigInt(lcInfo.adaAmount) + BigInt(lovelaceQty);
    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(BigInt(lcInfo.lcAmount));
    const newDatum = new ListData([newDatAda, newDatLC]);
    const minUTXOVal = new Value(BigInt(lovelaceQty) + maxTxFee + minChangeAmt);

    // Get wallet UTXOs
    const walletHelper = new WalletHelper(walletAPI);
    const utxos = await walletHelper.pickUtxos(minUTXOVal);
    console.log("utxos from Helios wallet helper", utxos);
 
    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // wallet regression testing
    const walletAPI2 = await window.cardano.nami.enable();

    const cborUtxos = await walletAPI2.getUtxos(bytesToHex(minUTXOVal.toCbor()));
    let Utxos = [];

    for (const cborUtxo of cborUtxos) {
      const _utxo = UTxO.fromCbor(hexToBytes(cborUtxo));
      Utxos.push(_utxo);
    }
    console.log("utxos direct from wallet API", Utxos);


    var cborColatUtxo2;
    if (whichWalletSelected == "eternl") {
      cborColatUtxo2 = await walletAPI2.experimental.getCollateral();
    } else if (whichWalletSelected == "nami") {
      cborColatUtxo2 = await walletAPI2.experimental.getCollateral();
    } else {
      throw console.error("No wallet selected")
    }

    if (cborColatUtxo2.length == 0) {
      throw console.error("No collateral set in wallet");
    }
    const colatUtxo2 = UTxO.fromCbor(hexToBytes(cborColatUtxo2[0]));
    console.log("colatUtxo2", colatUtxo2);


    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();
    console.log("colatUtxo", colatUtxo);

    // Start building the transaction
    const tx = new Tx();

    // Add the UTXO as inputs
    tx.addInputs(utxos[0]);

    // Construct the add Ada validator remdeemer
    const valRedeemer = new ConstrData(0, [])
    tx.addInput(valUtxo, valRedeemer);

    //const valRefUtxo = await getLCValRefUtxo();
    tx.addRefInput(
        valRefUtxo,
        compiledValScript
    );

    const newInlineDatum = Datum.inline(newDatum);
    const value = new Value(newAdaAmount.valueOf(), new Assets([
      [threadTokenMPH, [
          [hexToBytes(threadTokenName), BigInt(1)]
      ]]
    ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(lcValAddr, value, newInlineDatum));

    // Add the script as a witness to the transaction
    tx.attachScript(compiledRecMintScript);

    // Construct a receipt minting redeemer
    const mintRedeemer = new ConstrData(0, [new ByteArrayData(lcValHash.bytes)])
    const tokens: [number[], bigint][] = [[hexToBytes(recTokenName), BigInt(adaQty)]];

    tx.mintTokens(
      recTokenMPH,
      tokens,
      mintRedeemer
    )

    tx.addOutput(new TxOutput(
      changeAddr,
      new Value(minAda, new Assets([[recTokenMPH, tokens]]))
    ));

    tx.addCollateral(colatUtxo);

    // Specify when this transaction is valid from.   This is needed so
    // time is included in the transaction which will be use by the validator
    // script.  Add two hours for time to live and offset the current time
    // by 5 mins.
    const currentTime = new Date().getTime();
    const earlierTime = new Date(currentTime - 5 * 60 * 1000); 
    const laterTime = new Date(currentTime + 2 * 60 * 60 * 1000); 
   
    tx.validFrom(earlierTime);
    tx.validTo(laterTime);

    console.log("tx before final", tx.dump());

    // Send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());
    console.log("tx cbor", bytesToHex(tx.toCbor()));
    //console.log("tx body after final", tx.body.dump());
    console.log("unsigned tx body: ", bytesToHex(tx.body.toCbor()));

    console.log("Waiting for wallet signature...");
    const walletSig = await walletAPI2.signTx(bytesToHex(tx.toCbor()), true);
    console.log("walletSig", walletSig);


    console.log("Verifying signature...");
    const signatures = TxWitnesses.fromCbor(hexToBytes(walletSig)).signatures;
    console.log("TxWitness.signature", signatures);
    console.log("TxWitness.signature", signatures[0].dump());

    tx.addSignatures(signatures);

    //console.log("Verifying signature...");
    //const signatures = await walletAPI.signTx(tx);
    //tx.addSignatures(signatures);
    
    //console.log("Submitting transaction...");
    //const txHash = await walletAPI.submitTx(tx);
    //console.log("txHash", txHash.hex);
    //setTx({ txId: txHash.hex });

    //const txHash = await submitTx(tx);
    //console.log("txHash", txHash);
    //setTx({ txId: txHash});

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
            {walletIsEnabled && <div className={styles.border}><WalletInfo walletInfo={walletInfo}/></div>}
            {tx.txId && <div className={styles.border}><b>Transaction Success!!!</b>
            <p>TxId &nbsp;&nbsp;<a href={"https://preprod.cexplorer.io/tx/" + tx.txId} target="_blank" rel="noopener noreferrer" >{tx.txId}</a></p>
            <p>Please wait until the transaction is confirmed on the blockchain and reload this page before doing another transaction</p>
          </div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><AddAda onAddAda={addAda}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintLC onMintLC={mintLC}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><BurnLC onBurnLC={burnLC}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintMerchantToken onMintMerchantToken={mintMerchantToken}/></div>}

      </main>

      <footer className={styles.footer}>

      </footer>
    </div>
  )
}

export default Home
