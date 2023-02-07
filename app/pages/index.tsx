
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
  MintingPolicyHash, 
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
      const contractDirectory = path.join(process.cwd(), 'contracts/src');
      const fileContents = await fs.readFile(contractDirectory + '/lcValidator.hl', 'utf8');
      const contractScript = fileContents.toString();
      const compiledScript = Program.new(contractScript).compile(optimize);
      const valHash = compiledScript.validatorHash;
      const valAddr = Address.fromValidatorHash(valHash);
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
              lcValAddr: valAddr.toBech32(),
              lcValAdaAmt: utxo.amount[0].quantity, 
              lcRefTxId: utxo.tx_hash,
              lcRefTxIdx: utxo.output_index
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

  const lcValidatorScriptAddress = props.lcValAddr as string;
  const lcValAdaAmt = props.lcValAdaAmt as string;
  const lcValRefTxId = props.lcRefTxId as string;
  const lcValRefTxIdx = props.lcRefTxIdx as string;

  const optimize = false;
  const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
  const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  const threadTokenMPH = process.env.NEXT_PUBLIC_THREAD_TOKEN_MPH as string;
  const threadTokenName = process.env.NEXT_PUBLIC_THREAD_TOKEN_NAME as string;
  const lcTokenName = process.env.NEXT_PUBLIC_LC_TOKEN_NAME as string;
  const merchTokenMPH = process.env.NEXT_PUBLIC_MERCH_TOKEN_MPH as string;
  const merchTokenName = process.env.NEXT_PUBLIC_MERCH_TOKEN_NAME as string;
  const networkParamsUrl = process.env.NEXT_PUBLIC_NETWORK_PARAMS_URL as string;
  const ownerPkh = process.env.NEXT_PUBLIC_OWNER_PKH as string;
  const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;
  const lcSupply = process.env.NEXT_PUBLIC_LC_SUPPLY as string;

  const [lcInfo, setLCInfo] = useState(
    {
        address : '',
        adaAmount: 0,
        lcAmount: 0,
        ratio: 0,
    }
  );

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
          const datAddr : string = info?.address as string;
          
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
      const datAddr : string = info?.address as string;
      
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

    const blockfrostUrl : string = blockfrostAPI + "/addresses/" + lcValidatorScriptAddress + "/utxos/" + threadTokenMPH + threadTokenName;
    
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
    // TODO seach for lcAmount in payload
    const lcAmount = payload[0].amount[1].quantity;
    const mph = MintingPolicyHash.fromHex(threadTokenMPH);
    const ttToken = hexToBytes(threadTokenName);
    const lcToken = hexToBytes(lcTokenName);

    const value = new Value(BigInt(lovelaceAmount), new Assets([
        [mph, [
            [ttToken, BigInt(1)],
            [lcToken, BigInt(lcAmount)],

        ]]
    ]));

    return new UTxO(
      TxId.fromHex(payload[0].tx_hash),
      BigInt(payload[0].output_index),
      new TxOutput(
        Address.fromBech32(lcValidatorScriptAddress),
        value,
        Datum.inline(ListData.fromCbor(hexToBytes(payload[0].inline_datum)))
      )
    );
  }

  const getLCValRefUtxo = async () => {

    const response = await fetch('/api/lcValidator'); 
    const contractScript = await response.text();
    const compiledScript = Program.new(contractScript).compile(optimize);

    return new UTxO (
      TxId.fromHex(lcValRefTxId),
      BigInt(lcValRefTxIdx),
      new TxOutput(
        Address.fromBech32(lcValidatorScriptAddress),
        new Value(BigInt(lcValAdaAmt)),
        null,
        compiledScript
      )
    )
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
      return {datum: datObj, address: lcValidatorScriptAddress};
    } else {
      console.log("fetchLittercoin: thread token not found");
    }
  }

  const submitTx = async (tx: Tx) : Promise<string> => {
    const data = new Uint8Array(tx.toCbor());
    const url = blockfrostAPI + "/tx/submit";

    return new Promise((resolve, reject) => {
        const req = new XMLHttpRequest();
        req.onload = (_e) => {
            if (req.status == 200) {
                resolve(req.responseText.replaceAll('"',''));
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

  const mintLC = async (params : any) => {

    const address = params[0];
    const lcQty = params[1];
    const info = await fetchLittercoinInfo();
    const datObj = info?.datum;
    const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
    const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
    const newLCAmount : BigInt = BigInt(datLC) + BigInt(lcQty);
    const lcResAmount: BigInt = BigInt(lcSupply) - BigInt(newLCAmount.valueOf());
    const newDatAda = new IntData(BigInt(datAda));
    const newDatLC = new IntData(newLCAmount.valueOf());
    const newDatum = new ListData([newDatAda, newDatLC]);
    const valRedeemer = new ConstrData(
      1,
      [new ByteArrayData((Address.fromBech32(address)).pubKeyHash.bytes)]
    )
    const minAdaVal = new Value(BigInt(minAda));
    const cborUtxos = await walletAPI.getUtxos(bytesToHex(minAdaVal.toCbor()));
    let Utxos = [];

    for (const cborUtxo of cborUtxos) {
      const _utxo = UTxO.fromCbor(hexToBytes(cborUtxo));
      Utxos.push(_utxo);
    }

    var cborColatUtxo;
    if (whichWalletSelected == "eternl") {
      cborColatUtxo = await walletAPI.getCollateral();
    } else if (whichWalletSelected == "nami") {
      cborColatUtxo = await walletAPI.experimental.getCollateral();
    } else {
      throw console.error("No wallet selected")
    }

    if (cborColatUtxo.length == 0) {
      throw console.error("No collateral set in wallet");
    }
    const colatUtxo = UTxO.fromCbor(hexToBytes(cborColatUtxo[0]));
    const valScript = await fetch('/api/lcValidator'); 
    const valContractScript = await valScript.text();
    const valCompiledScript = Program.new(valContractScript).compile(optimize);
    const valAddr = Address.fromValidatorHash(valCompiledScript.validatorHash);

    // Get the change address from the wallet
    const hexChangeAddr = await walletAPI.getChangeAddress();
    const changeAddr = Address.fromHex(hexChangeAddr);

    // Start building the transaction
    const tx = new Tx();

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }
    const valUtxo = await getTTUtxo();
    tx.addInput(valUtxo, valRedeemer);

    const valRefUtxo = await getLCValRefUtxo();
    tx.addRefInput(
        valRefUtxo,
        valCompiledScript
    );

    const newInlineDatum = Datum.inline(newDatum);
    const value = new Value(BigInt(datAda), new Assets([
      [MintingPolicyHash.fromHex(threadTokenMPH), [
        [hexToBytes(threadTokenName), BigInt(1)],
        [hexToBytes(lcTokenName), BigInt(lcResAmount.valueOf())]
      ]]
    ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(valAddr, value, newInlineDatum));
    const tokens: [number[], bigint][] = [[hexToBytes(lcTokenName), BigInt(lcQty)]];

    tx.addOutput(new TxOutput(
      Address.fromBech32(address),
      new Value(BigInt(minAda), new Assets([[MintingPolicyHash.fromHex(threadTokenMPH), tokens]]))
    ));

    tx.addCollateral(colatUtxo);
    tx.addSigner(PubKeyHash.fromHex(ownerPkh));

    const networkParams = new NetworkParams(
      await fetch(networkParamsUrl)
          .then(response => response.json())
    )
    console.log("tx before final", tx.dump());

    // send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());
    console.log("Waiting for wallet signature...");
    const walletSig = await walletAPI.signTx(bytesToHex(tx.toCbor()), true)
    console.log("Verifying signature...");
    const signatures = TxWitnesses.fromCbor(hexToBytes(walletSig)).signatures
    tx.addSignatures(signatures)
    console.log("Submitting transaction...");

    //const txHash = await walletAPI.submitTx(bytesToHex(tx.toCbor()));
    const txHash = await submitTx(tx);
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
   } 

  const burnLC = async (lcQty : number) => {
  
    const info = await fetchLittercoinInfo();
    const datObj = info?.datum;
    const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
    const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
    const ratio = Math.floor(datAda / datLC);
    const withdrawAda : number = lcQty * ratio;
    const newLCAmount : BigInt = BigInt(datLC) - BigInt(lcQty);
    const newAdaAmount : BigInt = BigInt(datAda) - BigInt(withdrawAda);
    const lcResAmount: BigInt = BigInt(lcSupply) - BigInt(newLCAmount.valueOf());

    // Get the change address from the wallet
    const hexChangeAddr = await walletAPI.getChangeAddress();
    const changeAddr = Address.fromHex(hexChangeAddr);
    const valRedeemer = new ConstrData(
      2,
      [new ByteArrayData(changeAddr.pubKeyHash.bytes)]
    )
    const minAdaVal = new Value(BigInt(minAda));
    const cborUtxos = await walletAPI.getUtxos(bytesToHex(minAdaVal.toCbor()));
    let Utxos = [];

    for (const cborUtxo of cborUtxos) {
      const _utxo = UTxO.fromCbor(hexToBytes(cborUtxo));
      Utxos.push(_utxo);
    }

    var cborColatUtxo;
    if (whichWalletSelected == "eternl") {
      cborColatUtxo = await walletAPI.getCollateral();
    } else if (whichWalletSelected == "nami") {
      cborColatUtxo = await walletAPI.experimental.getCollateral();
    } else {
      throw console.error("No wallet selected")
    }

    if (cborColatUtxo.length == 0) {
      throw console.error("No collateral set in wallet");
    }
    const colatUtxo = UTxO.fromCbor(hexToBytes(cborColatUtxo[0]));
    const valScript = await fetch('/api/lcValidator'); 
    const valContractScript = await valScript.text();
    const valCompiledScript = Program.new(valContractScript).compile(optimize);
    const valAddr = Address.fromValidatorHash(true, valCompiledScript.validatorHash);

    // Start building the transaction
    const tx = new Tx();

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }
    const valUtxo = await getTTUtxo();
    tx.addInput(valUtxo, valRedeemer);
    const valRefUtxo = await getLCValRefUtxo();
    tx.addRefInput(
        valRefUtxo,
        valCompiledScript
    );

    const newDatAda = new IntData(BigInt(newAdaAmount.valueOf()));
    const newDatLC = new IntData(newLCAmount.valueOf());
    const newDatum = new ListData([newDatAda, newDatLC]);
    const newInlineDatum = Datum.inline(newDatum);

    if (newAdaAmount < BigInt(minAda)) {
      const value = new Value(BigInt(minAda), new Assets([
        [MintingPolicyHash.fromHex(threadTokenMPH), [
          [hexToBytes(threadTokenName), BigInt(1)],
          [hexToBytes(lcTokenName), BigInt(lcResAmount.valueOf())]
        ]]
      ]));
      // send Ada, updated dautm and thread token back to script address
      tx.addOutput(new TxOutput(valAddr, value, newInlineDatum));
    } else {
      const value = new Value(BigInt(newAdaAmount.valueOf()), new Assets([
        [MintingPolicyHash.fromHex(threadTokenMPH), [
          [hexToBytes(threadTokenName), BigInt(1)],
          [hexToBytes(lcTokenName), BigInt(lcResAmount.valueOf())]
        ]]
      ]));
      // send Ada, updated dautm and thread token back to script address
      tx.addOutput(new TxOutput(valAddr, value, newInlineDatum));
    }

    const tokens: [number[], bigint][] = [[hexToBytes(merchTokenName), BigInt(1)]];

    tx.addOutput(new TxOutput(
      changeAddr,
      new Value(BigInt(withdrawAda), new Assets([[MintingPolicyHash.fromHex(merchTokenMPH), tokens]]))
    ));

    tx.addCollateral(colatUtxo);

    const networkParams = new NetworkParams(
      await fetch(networkParamsUrl)
          .then(response => response.json())
    )
    console.log("tx before final", tx.dump());

    // send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());
    console.log("Waiting for wallet signature...");
    const walletSig = await walletAPI.signTx(bytesToHex(tx.toCbor()), true)
    console.log("Verifying signature...");
    const signatures = TxWitnesses.fromCbor(hexToBytes(walletSig)).signatures
    tx.addSignatures(signatures)
    console.log("Submitting transaction...");

    //const txHash = await walletAPI.submitTx(bytesToHex(tx.toCbor()));
    const txHash = await submitTx(tx);
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  } 


  const mintMerchantToken = async (merchAddress : string) => {

    const mintScript =`
    minting signed

    const OWNER_PKH: ByteArray = #` + ownerPkh + `
    const ownerPkh: PubKeyHash = PubKeyHash::new(OWNER_PKH)

    func main(ctx: ScriptContext) -> Bool {
        ctx.tx.is_signed_by(ownerPkh)
    }`;

    const mintProgram = Program.new(mintScript).compile(optimize);
    const minAdaVal = new Value(BigInt(minAda));
    const cborUtxos = await walletAPI.getUtxos(bytesToHex(minAdaVal.toCbor()));
    let Utxos = [];

    for (const cborUtxo of cborUtxos) {
      const _utxo = UTxO.fromCbor(hexToBytes(cborUtxo));
      Utxos.push(_utxo);
    }

    var cborColatUtxo;
    if (whichWalletSelected == "eternl") {
      cborColatUtxo = await walletAPI.getCollateral();
    } else if (whichWalletSelected == "nami") {
      cborColatUtxo = await walletAPI.experimental.getCollateral();
    } else {
      throw console.error("No wallet selected")
    }

    if (cborColatUtxo.length == 0) {
      throw console.error("No collateral set in wallet");
    }
    const colatUtxo = UTxO.fromCbor(hexToBytes(cborColatUtxo[0]));

    // Get the change address from the wallet
    const hexChangeAddr = await walletAPI.getChangeAddress();
    const changeAddr = Address.fromHex(hexChangeAddr);

    // Start building the transaction
    const tx = new Tx();

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }

    tx.attachScript(mintProgram);
    const merchTokenName = ByteArrayData.fromString("Merchant Token Littercoin").toHex();
    const tokens: [number[], bigint][] = [[hexToBytes(merchTokenName), BigInt(1)]];
    const mintRedeemer = new ConstrData(0, []);

    tx.mintTokens(
      mintProgram.mintingPolicyHash,
      tokens,
      mintRedeemer
    )

    tx.addOutput(new TxOutput(
      Address.fromBech32(merchAddress),
      new Value(BigInt(minAda), new Assets([[mintProgram.mintingPolicyHash, tokens]]))
    ));

    tx.addCollateral(colatUtxo);
    tx.addSigner(PubKeyHash.fromHex(ownerPkh));

    const networkParams = new NetworkParams(
      await fetch(networkParamsUrl)
          .then(response => response.json())
    )
    console.log("tx before final", tx.dump());

    // send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());
    console.log("Waiting for wallet signature...");
    const walletSig = await walletAPI.signTx(bytesToHex(tx.toCbor()), true)
    console.log("Verifying signature...");
    const signatures = TxWitnesses.fromCbor(hexToBytes(walletSig)).signatures
    tx.addSignatures(signatures)
    console.log("Submitting transaction...");

    //const txHash = await walletAPI.submitTx(bytesToHex(tx.toCbor()));
    const txHash = await submitTx(tx);
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  }   

  const addAda = async (adaQty : any) => {

    const info = await fetchLittercoinInfo();
    const datObj = info?.datum;
    const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
    const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
    const lcResAmount: BigInt = BigInt(lcSupply) - BigInt(datLC);
    const newAdaAmount : BigInt = BigInt(datAda) + BigInt(adaQty*1000000);
    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(BigInt(datLC));
    const newDatum = new ListData([newDatAda, newDatLC]);
    const valRedeemer = new ConstrData(0, []);
    const adaAmountVal = new Value(BigInt((adaQty)*1000000));

    // Get wallet UTXOs
    const walletHelper = new WalletHelper(walletAPI);
    const utxos = await walletHelper.pickUtxos(adaAmountVal);

    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();

    // Compile the Helios script 
    const script = await fetch('/api/lcValidator'); 
    const contractScript = await script.text();
    const compiledScript = Program.new(contractScript).compile(optimize);

    // Extract the validator script address
    const valAddr = Address.fromValidatorHash(compiledScript.validatorHash);

    // Start building the transaction
    const tx = new Tx();

    // Add the UTXO as inputs
    tx.addInputs(utxos[0]);

    // Add the spare UTXOs as inputs in case the UTXO selected is an exact
    // Ada amount match and there is not enough funds to cover fees
    tx.addInputs(utxos[1]);

    // Get thread token UTXO and add as an input
    const valUtxo = await getTTUtxo();
    tx.addInput(valUtxo, valRedeemer);

    // Get littercoin smart contract reference UTXO and add as an input
    const valRefUtxo = await getLCValRefUtxo();
    tx.addRefInput(
        valRefUtxo,
        compiledScript   // adding compiled script so it can be evaluated locally 
    );

    // Construct the inline datum
    const newInlineDatum = Datum.inline(newDatum);
    const value = new Value(newAdaAmount.valueOf(), new Assets([
      [MintingPolicyHash.fromHex(threadTokenMPH), [
          [hexToBytes(threadTokenName), BigInt(1)],
          [hexToBytes(lcTokenName), BigInt(lcResAmount.valueOf())]
      ]]
    ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(valAddr, value, newInlineDatum));
    tx.addCollateral(colatUtxo);

    const networkParams = new NetworkParams(
      await fetch(networkParamsUrl)
          .then(response => response.json())
    )
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
