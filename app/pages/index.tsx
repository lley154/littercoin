
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
import WalletInfo from '../components/WalletInfo';

import {
  Address, 
  Assets, 
  bytesToHex, 
  ByteArrayData,
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
  UTxO} from "@hyperionbt/helios";

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
      const valFile = await fs.readFile(contractDirectory + '/lcValidator.hl', 'utf8');
      const valScript = valFile.toString();
      const compiledValScript = Program.new(valScript).compile(optimize);
      const valHash = compiledValScript.validatorHash; 
      const valAddr = Address.fromValidatorHash(valHash);
      const mintFile = await fs.readFile(contractDirectory + '/lcMint.hl', 'utf8');
      const mintScript = mintFile.toString();
      const threadTokenFile = await fs.readFile(contractDirectory + '/threadToken.hl', 'utf8');
      const threadTokenScript = threadTokenFile.toString();

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
              ttMintScript: threadTokenScript
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
  const lcValScript = props.lcValScript as string;
  const compiledValScript = Program.new(lcValScript).compile(optimize);
  const lcValHash = compiledValScript.validatorHash; 
  const lcValAddr = Address.fromValidatorHash(lcValHash);

  const lcMintScript = props.lcMintScript as string;
  const compiledLCMintScript = Program.new(lcMintScript).compile(optimize);
  const lcTokenMPH = compiledLCMintScript.mintingPolicyHash;

  const ttMintScript = props.ttMintScript as string;
  const compiledTTMintScript = Program.new(ttMintScript).compile(optimize);
  const threadTokenMPH = compiledTTMintScript.mintingPolicyHash;
  
  const lcValAdaAmt = props.lcValAdaAmt as string;
  const lcValRefTxId = props.lcRefTxId as string;
  const lcValRefTxIdx = props.lcRefTxIdx as string;

  const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
  const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  //const threadTokenMPH = process.env.NEXT_PUBLIC_THREAD_TOKEN_MPH as string;
  const threadTokenName = process.env.NEXT_PUBLIC_THREAD_TOKEN_NAME as string;
  //const lcTokenMPH = process.env.NEXT_PUBLIC_LC_TOKEN_MPH as string;
  const lcTokenName = process.env.NEXT_PUBLIC_LC_TOKEN_NAME as string;
  //const lcMintAddr = process.env.NEXT_PUBLIC_LC_MINT_ADDR as string;
  const networkParamsUrl = process.env.NEXT_PUBLIC_NETWORK_PARAMS_URL as string;
  const ownerPkh = process.env.NEXT_PUBLIC_OWNER_PKH as string;
  const minAda = process.env.NEXT_PUBLIC_MIN_ADA as string;
  const serviceFee = process.env.NEXT_PUBLIC_MAX_SERVICE_FEE as string;

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
    //const mph = MintingPolicyHash.fromHex(threadTokenMPH);
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

    //const response = await fetch('/api/lcValidator'); 
    //const contractScript = await response.text();
    //const compiledScript = Program.new(contractScript).compile(optimize);

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
    console.log("setRefUTXO", valRefUTXO);
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

    let walletwalletAPI = undefined;
      try {
        const walletChoice = whichWalletSelected;
        if (walletChoice === "nami") {
            walletwalletAPI = await window.cardano.nami.enable();
        } else if (walletChoice === "eternl") {
            walletwalletAPI = await window.cardano.eternl.enable(); 
        } 
        return walletwalletAPI 
    } catch (err) {
        console.log('enableWallet error', err);
    }
  }

  const getBalance = async () => {
    try {
        const balanceCBORHex = await walletAPI.getBalance();
        const balanceAmountValue =  Value.fromCbor(hexToBytes(balanceCBORHex));
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
    //const info = await fetchLittercoinInfo();
    //const datObj = info?.datum;
    //const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
    //const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
    //const newLCAmount : BigInt = BigInt(datLC) + BigInt(lcQty);
    //const newDatAda = new IntData(BigInt(datAda));
    const newLCAmount : BigInt = BigInt(lcInfo.lcAmount) + BigInt(lcQty);
    const newDatAda = new IntData(BigInt(lcInfo.adaAmount));
    const newDatLC = new IntData(newLCAmount.valueOf());
    const newDatum = new ListData([newDatAda, newDatLC]);

    const valRedeemer = new ConstrData(
      1,
      []
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

    //const valScript = await fetch('/api/lcValidator'); 
    //const valContractScript = await valScript.text();
    //const valCompiledScript = Program.new(valContractScript).compile(optimize);
    //const valAddr = Address.fromValidatorHash(valCompiledScript.validatorHash);

    // Get the change address from the wallet
    const hexChangeAddr = await walletAPI.getChangeAddress();
    const changeAddr = Address.fromHex(hexChangeAddr);

    // Start building the transaction
    const tx = new Tx();

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }
    //const valUtxo = await getTTUtxo();
    //tx.addInput(valUtxo, valRedeemer);
    tx.addInput(valUtxo, valRedeemer);

    //const valRefUtxo = await getLCValRefUtxo();
    tx.addRefInput(
        valRefUtxo,
        compiledValScript
    );

    //const mintScript = await fetch('/api/lcMint'); 
    //const mintContractScript = await mintScript.text();
    //console.log("prettyIR", Program.new(mintContractScript).prettyIR());
    //const mintCompiledScript = Program.new(mintContractScript).compile(optimize);
    //const mintRefUtxo = await getLCMintRefUtxo();
/*
    tx.addRefInput(
      mintRefUtxo,
      mintCompiledScript
  );
*/

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

    const mintRedeemer = new ConstrData(
      0,
      [new ByteArrayData(lcValHash.bytes)]
    )
    const tokens: [number[], bigint][] = [[hexToBytes(lcTokenName), BigInt(lcQty)]];

    tx.mintTokens(
      lcTokenMPH,
      tokens,
      mintRedeemer
    )

    tx.addOutput(new TxOutput(
      Address.fromBech32(address),
      new Value(BigInt(minAda), new Assets([[lcTokenMPH, tokens]]))
    ));

    tx.addCollateral(colatUtxo);

    tx.addSigner(PubKeyHash.fromHex(ownerPkh));

    ///const networkParams = new NetworkParams(
    //  await fetch(networkParamsUrl)
    //      .then(response => response.json())
    //)
    console.log("tx before final", tx.dump());

    // send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Waiting for wallet signature...");
    const walletSig = await walletAPI.signTx(bytesToHex(tx.toCbor()), true)

    //console.log("unsigned tx: ", bytesToHex(tx.toCbor()));

    console.log("Verifying signature...");
    const signatures = TxWitnesses.fromCbor(hexToBytes(walletSig)).signatures
    tx.addSignatures(signatures)

    console.log("Submitting transaction...");
    //console.log("signed tx: ", bytesToHex(tx.toCbor()));
    const txHash = await walletAPI.submitTx(bytesToHex(tx.toCbor()));
    //const txHash = await submitTx(tx);
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
   } 


  const burnLC = async (lcQty : any) => {
    return true;
  } 


  const mintMerchantToken = async (merchAddress : string) => {
    return true;
  }   

  const mintOwnerToken = async (ownerAddress : any) => {
    return true;
  }  

  const addAda = async (adaQty : any) => {

    //const info = await fetchLittercoinInfo();
    //const datObj = info?.datum;
    //const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
    //const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
    const newAdaAmount : BigInt = BigInt(lcInfo.adaAmount) + BigInt(adaQty*1000000);
    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(BigInt(lcInfo.lcAmount));
    const newDatum = new ListData([newDatAda, newDatLC]);

    const valRedeemer = new ConstrData(
      0,
      []
    )

    const adaAmountVal = new Value(BigInt((adaQty)*1000000));
    const cborUtxos = await walletAPI.getUtxos(bytesToHex(adaAmountVal.toCbor()));
 
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

    //const script = await fetch('/api/lcValidator'); 
    //const contractScript = await script.text();
    //console.log("prettyIR", Program.new(contractScript).prettyIR());

    //const compiledScript = Program.new(contractScript).compile(optimize);
    //const valAddr = Address.fromValidatorHash(compiledScript.validatorHash);

    // Get the change address from the wallet
    const hexChangeAddr = await walletAPI.getChangeAddress();
    const changeAddr = Address.fromHex(hexChangeAddr);

    // Start building the transaction
    const tx = new Tx();

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }

    //const valUtxo = await getTTUtxo();
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
    tx.addCollateral(colatUtxo);

    //const networkParams = new NetworkParams(
    //  await fetch(networkParamsUrl)
    //      .then(response => response.json())
    //)
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
    const txHash = await walletAPI.submitTx(bytesToHex(tx.toCbor()));
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
            {walletIsEnabled && <div className={styles.border}><WalletInfo walletInfo={walletInfo}/></div>}
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
