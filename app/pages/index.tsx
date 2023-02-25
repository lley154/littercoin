
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
import LoadingSpinner from '../components/LoadingSpinner';

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
  Tx, 
  UTxO,
  WalletHelper, 
  MintingPolicyHash} from "@hyperionbt/helios";

  import path from 'path';
  import { promises as fs } from 'fs';
  import axios from 'axios';

  declare global {
    interface Window {
        cardano:any;
    }
  }

  async function getUtxos(blockfrostUrl: string) {

    const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;

    try {
       let res = await axios({
            url: blockfrostUrl,
            method: 'get',
            timeout: 8000,
            headers: {
                'Content-Type': 'application/json',
                'project_id': apiKey
            }
        })
        if(res.status == 200){
            return res.data;
        } else {
          console.log("getUtxos: error getting utxos from blockfrost", res);
          return res.data;
        }   
    }
    catch (err) {
        console.error("getUtxos: error getting utxos from blockfrost", err);
    }
}

  export async function getServerSideProps() {
  
    // set in env variables
    const optimize = false;
    const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
    const networkParamsFilePath = process.env.NEXT_PUBLIC_NETWORK_PARAMS_FILE as string;

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

      // Littercoin rewards token minting script
      const rewardsTokenFile = await fs.readFile(contractDirectory + '/rewardsToken.hl', 'utf8');
      const rewardsTokenScript = rewardsTokenFile.toString();

      // Network Parameters
      const networkParamsFile = await fs.readFile(contractDirectory + '/' + networkParamsFilePath, 'utf8');
      const networkParams = networkParamsFile.toString();

      const blockfrostUrl : string = blockfrostAPI + "/addresses/" + valAddr.toBech32() + "/utxos/?order=asc";
      console.log("blockfrostUrl: ", blockfrostUrl);

      let utxos = await getUtxos(blockfrostUrl);

      // Find the reference utxo with the correct validator hash
      if (utxos.length > 0) {
        for (var utxo of utxos) {
          if (utxo.reference_script_hash === valHash.hex) {
            const lcVal = {
              lcValScript: valScript,
              lcValAdaAmt: utxo.amount[0].quantity, 
              lcRefTxId: utxo.tx_hash,
              lcRefTxIdx: utxo.output_index,
              lcMintScript: mintScript,
              ttMintScript: threadTokenScript,
              mtMintScript: merchTokenScript,
              recMintScript: rewardsTokenScript,
              network: networkParams
            }
            return { props: lcVal }
          }
        }
      } else {
        throw console.error("littercoin validator reference utxo not found")
      }
    } catch (err) {
      console.log('getServerSideProps error: ', err);
    } 
    // No valid reference utxo found
    return { props: {} };
  }


const Home: NextPage = (props: any) => {

  // Set the Helios compiler optimizer flag
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

  // Littercoin donation rewards minting script
  const recMintScript = props.recMintScript as string;
  const compiledRecMintScript = Program.new(recMintScript).compile(optimize);
  const rewardsTokenMPH = compiledRecMintScript.mintingPolicyHash;
  
  // Littercoin reference UTXO
  const lcValAdaAmt = props.lcValAdaAmt as string;
  const lcValRefTxId = props.lcRefTxId as string;
  const lcValRefTxIdx = props.lcRefTxIdx as string;

  // Network Params
  //const networkParams = props.network as string;
  const networkParams = new NetworkParams(JSON.parse(props.network as string));

  const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
  const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  const threadTokenName = process.env.NEXT_PUBLIC_THREAD_TOKEN_NAME as string;
  const lcTokenName = process.env.NEXT_PUBLIC_LC_TOKEN_NAME as string;
  const merchTokenName = process.env.NEXT_PUBLIC_MERCH_TOKEN_NAME as string;
  const rewardsTokenName = process.env.NEXT_PUBLIC_REWARDS_TOKEN_NAME as string;
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
  const [whichWalletSelected, setWhichWalletSelected] = useState(undefined);
  const [walletIsEnabled, setWalletIsEnabled] = useState(false);
  const [walletAPI, setWalletAPI] = useState<undefined | any>(undefined);
  const [walletInfo, setWalletInfo] = useState({ balance : ''});
  const [isLoading, setIsLoading] = useState(false);
  const [tx, setTx] = useState({ txId : '' });
  

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

  // Get the utxo with the thread token at the LC validator address
  const getTTUtxo = async () => {

    const blockfrostUrl : string = blockfrostAPI + "/addresses/" + lcValAddr.toBech32() + "/utxos/" + threadTokenMPH.hex + threadTokenName;
    console.log("blockfrostUrl", blockfrostUrl);

    let utxos = await getUtxos(blockfrostUrl);
    if (utxos.length == 0) {
      throw console.error("thread token not found")
    }
    const lovelaceAmount = utxos[0].amount[0].quantity;
    const token = hexToBytes(threadTokenName);
    const value = new Value(BigInt(lovelaceAmount), new Assets([
        [threadTokenMPH, [
            [token, BigInt(1)]
        ]]
    ]));

    const ttUtxo = new UTxO(
      TxId.fromHex(utxos[0].tx_hash),
      BigInt(utxos[0].output_index),
      new TxOutput(
        lcValAddr,
        value,
        Datum.inline(ListData.fromCbor(hexToBytes(utxos[0].inline_datum)))
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

  const submitTx = async (tx: Tx) : Promise<string> => {
    const data = new Uint8Array(tx.toCbor());
    const url = blockfrostAPI + "/tx/submit";
    const config = {
      headers:{
        "content-type": "application/cbor",
        "project_id": apiKey
      }
    };
    let txHash = "";
    try {
        await axios.post(url, data, config)
        .then(function (response) {
          txHash = response.data;
        })
        .catch(function (error) {
          throw console.error("submitTx error: " + error);
        });
    } catch (error) {
        console.error(error);
        throw console.error("submitTx error: " + error);
    }
    return txHash;
  }

  // Get the number of tokens in a set of utxo for a given mph
  const tokenCount = async (tokenMph: MintingPolicyHash, utxos: UTxO[]) : Promise<BigInt> => {
    let tokenCount = BigInt(0);
    for (const utxo of utxos) {
      const mphs = utxo.value.assets.mintingPolicies;
      for (const mph of mphs) {
        if (mph.hex == tokenMph.hex) {
          const tokenNames = utxo.value.assets.getTokenNames(mph);
          for (const tokenName of tokenNames) {
            tokenCount += utxo.value.assets.get(mph, tokenName);
          }
        }
      }
    }
    return tokenCount;
  }

  const mintLC = async (params : any) => {

    setIsLoading(true);
    // re-enable wallet api if the wallet account has been changed
    const api = await enableWallet();
    setWalletAPI(api);

    const address = params[0];
    const lcQty = params[1];
    const newLCAmount = BigInt(lcInfo.lcAmount) + BigInt(lcQty);
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

    // Check the total number of littercoin already in the utxos.
    // We will then add this number to the minted amount
    // so we can put the total amount of littercoins in the output.
    const lcTokenCount = await tokenCount(lcTokenMPH, utxos[0]);

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

    // Construct the amount of littercoin tokens to mint
    const mintTokens: [number[], bigint][] = [[hexToBytes(lcTokenName), BigInt(lcQty)]];
    
    // Add the mint to the tx
    tx.mintTokens(
      lcTokenMPH,
      mintTokens,
      mintRedeemer
    )

    // Construct the total amount of littercoin tokens
    const lcTokens: [number[], bigint][] = [[hexToBytes(lcTokenName), 
                                            (BigInt(lcQty) + lcTokenCount.valueOf())]];
    
    // Attached the output with the minted littercoins to the destination address
    tx.addOutput(new TxOutput(
      Address.fromBech32(address),
      new Value(minAda, new Assets([[lcTokenMPH, lcTokens]]))
    ));

    tx.addCollateral(colatUtxo);

    // Add owner pkh as a signer which is required to mint littercoin
    tx.addSigner(PubKeyHash.fromHex(ownerPkh));

    console.log("tx before final", tx.dump());

    // Send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    // Get back-end signature of owner private key and submit tx
    console.log("Get Back-end to sign...");
    const response = await fetch('/api/getSignature', {
      method: 'POST',
      body: JSON.stringify({ txCbor: bytesToHex(tx.toCbor()) }),
      headers: {
        'Content-type' : 'application/json'
      },
    }) 

    console.log("Submitting transaction...");
    const txHash = await response.json();

    if (response.status == 200) {
      setIsLoading(false); 
      console.log("txHash", txHash);
      setTx({ txId: txHash });
    } else {
      setIsLoading(false); 
      console.log("Mint Littercoin Failed: " + txHash);
    }
   } 

  const burnLC = async (lcQty : any) => {

    setIsLoading(true);
    // re-enable wallet api if the wallet account has been changed
    const api = await enableWallet();
    setWalletAPI(api);
    
    const lcQtyAbs:number = Math.abs(Number(lcQty));
    const newLCAmount = BigInt(lcInfo.lcAmount) - BigInt(lcQtyAbs);
    const ratio: number = Math.floor(lcInfo.adaAmount / lcInfo.lcAmount);
    const withdrawAda: number = Number(ratio) * lcQtyAbs;
    const adaDiff: number = lcInfo.adaAmount - withdrawAda;
    var newAdaAmount: BigInt;
    if (adaDiff >= minAda) {
      newAdaAmount = BigInt(lcInfo.adaAmount) - BigInt(withdrawAda);
    } else {
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

    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // Get unused addresses if available
    let unusedAddr:Address[] = [];
    const unusedAddresses: Address[] = await walletAPI.unusedAddresses;
    if (unusedAddresses.length < 3) {
      unusedAddr.push(changeAddr);
      unusedAddr.push(changeAddr);
      unusedAddr.push(changeAddr);
    } else {
      unusedAddr = unusedAddresses;
    }

    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();

    // Check the total number of littercoin in the utxos.
    // We will then decrement the number of tokens being burned
    // and put the rest (if any) in an output.
    const lcTokenCount = await tokenCount(lcTokenMPH, utxos[0]);

    // Start building the transaction
    const tx = new Tx();

    tx.addInputs(utxos[0]);

    // Construct the burn littercoin validator redeemer
    const valRedeemer = new ConstrData(2, [new ByteArrayData(unusedAddr[0].pubKeyHash.bytes)])
    tx.addInput(valUtxo, valRedeemer);

    tx.addRefInput(
        valRefUtxo,
        compiledValScript
    );

    const newInlineDatum = Datum.inline(newDatum);
    const outputValue = new Value(newAdaAmount.valueOf(), 
                                  new Assets([[threadTokenMPH, [
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

    // Make sure the output address matches the pkh provided in the validator redeemer above
    const merchAdaVal: Value = new Value(BigInt(minAda), new Assets([[merchTokenMPH, merchTokens]]));
    tx.addOutput(new TxOutput(
      unusedAddr[0],
      merchAdaVal
    ));

    // Construct the ada withdraw amount
    const withdrawAdaVal: Value = new Value(BigInt(withdrawAda));

    tx.addOutput(new TxOutput(
      unusedAddr[1],
      withdrawAdaVal
    ));

    // Construct the littercoin tokens to be returned if any
    const lcDelta = lcTokenCount.valueOf() - BigInt(lcQty);
    if (lcDelta > 0) {

      const lcTokens: [number[], bigint][] = [[hexToBytes(lcTokenName), lcDelta]];
      const lcVal: Value = new Value(BigInt(minAda), new Assets([[lcTokenMPH, lcTokens]]));
      
      tx.addOutput(new TxOutput(
        unusedAddr[2],
        lcVal
      ));
    } 

    tx.addCollateral(colatUtxo);

    console.log("tx before final", tx.dump());

    // Send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    console.log("Submitting transaction...");
    const txHash = await submitTx(tx);
    setIsLoading(false); 
    console.log("txHash", txHash);
    setTx({ txId: txHash });
   } 


  const mintMerchantToken = async (merchAddress : string) => {

    setIsLoading(true);
    // re-enable wallet api if the wallet account has been changed
    const api = await enableWallet();
    setWalletAPI(api);
    
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
    console.log("network", networkParams);

    // Send any change back to the wallet
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    console.log("Submitting transaction...");
    const txHash = await submitTx(tx);
    setIsLoading(false); 
    console.log("txHash", txHash);
    setTx({ txId: txHash });
  }   

  const addAda = async (adaQty : any) => {

    setIsLoading(true);
    // re-enable wallet api if the wallet account has been changed
    const api = await enableWallet();
    setWalletAPI(api);
    
    const lovelaceQty =  Number(adaQty) * 1000000;
    if (lovelaceQty < minAda) {
      throw console.error("Minimum donation is 2 Ada");
    }
    const newAdaAmount : BigInt = BigInt(lcInfo.adaAmount) + BigInt(lovelaceQty);
    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(BigInt(lcInfo.lcAmount));
    const newDatum = new ListData([newDatAda, newDatLC]);
    const minUTXOVal = new Value(BigInt(lovelaceQty) + minAda + maxTxFee + minChangeAmt);

    // Get wallet UTXOs
    const walletHelper = new WalletHelper(walletAPI);
    const utxos = await walletHelper.pickUtxos(minUTXOVal);

    // See if there are any previous rewards tokens already minted
    // in the utxos. If so, then they need to be added to the
    // the final output.
    const rewardsTokenCount = await tokenCount(rewardsTokenMPH, utxos[0]);

    // Get change address
    const changeAddr = await walletHelper.changeAddress;

    // Get unused addresses if available
    var unusedAddr: Address;
    const unusedAddrs: Address[] = await walletAPI.unusedAddresses;
    if (unusedAddrs.length == 0) {
      unusedAddr = changeAddr;
    } else {
      unusedAddr = unusedAddrs[0];
    }

    // Determine the UTXO used for collateral
    const colatUtxo = await walletHelper.pickCollateral();

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
    // Construct output value with inline datum
    const newInlineDatum = Datum.inline(newDatum);
    const value = new Value(newAdaAmount.valueOf(), new Assets([
      [threadTokenMPH, [
          [hexToBytes(threadTokenName), BigInt(1)]
      ]]
    ]));

    // Send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(lcValAddr, value, newInlineDatum));

    // Add the script as a witness to the transaction
    tx.attachScript(compiledRecMintScript);

    // Construct a littercoin rewards minting redeemer
    const mintRedeemer = new ConstrData(0, [new ByteArrayData(lcValHash.bytes)])
    const mintTokens: [number[], bigint][] = [[hexToBytes(rewardsTokenName), 
                                               BigInt(adaQty)]];

    tx.mintTokens(
      rewardsTokenMPH,
      mintTokens,
      mintRedeemer
    )

    const tokens: [number[], bigint][] = [[hexToBytes(rewardsTokenName), 
      (BigInt(adaQty) + rewardsTokenCount.valueOf())]];
 
    tx.addOutput(new TxOutput(
      unusedAddr,
      new Value(minAda, new Assets([[rewardsTokenMPH, tokens]]))
    ));

    tx.addCollateral(colatUtxo);
    console.log("tx before final", await tx.dump());

    // Send any change back to the wallet
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", await tx.dump());

    console.log("Verifying signature...");
    const signatures = await walletAPI.signTx(tx);
    tx.addSignatures(signatures);

    console.log("Submitting transaction...");
    //const txHash = await walletAPI.submitTx(tx);
    const txHash = await submitTx(tx);
    setIsLoading(false); 
    console.log("txHash", txHash);
    setTx({ txId: txHash });
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
            {isLoading && <LoadingSpinner />}
            {tx.txId && <div className={styles.border}><b>Transaction Success!!!</b>
            <p>TxId &nbsp;&nbsp;<a href={"https://preprod.cexplorer.io/tx/" + tx.txId} target="_blank" rel="noopener noreferrer" >{tx.txId}</a></p>
            <p>Please wait until the transaction is confirmed on the blockchain and reload this page before doing another transaction</p>
          </div>}
          {walletIsEnabled && !tx.txId && !isLoading && <div className={styles.border}><AddAda onAddAda={addAda}/></div>}
          {walletIsEnabled && !tx.txId && !isLoading && <div className={styles.border}><MintLC onMintLC={mintLC}/></div>}
          {walletIsEnabled && !tx.txId && !isLoading && <div className={styles.border}><BurnLC onBurnLC={burnLC}/></div>}
          {walletIsEnabled && !tx.txId && !isLoading && <div className={styles.border}><MintMerchantToken onMintMerchantToken={mintMerchantToken}/></div>}

      </main>

      <footer className={styles.footer}>

      </footer>
    </div>
  )
}

export default Home
