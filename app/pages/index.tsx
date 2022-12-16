
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
  TxRefInput, 
  TxWitnesses,
  Tx, 
  UTxO} from "@lley/helios";

  import path from 'path';
  import { promises as fs } from 'fs';


  export async function getServerSideProps() {
  
    // set in env variables
    const optimize = false;
    const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
    const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;

    try {
      //Find the absolute path of the json directory
      const contractDirectory = path.join(process.cwd(), 'contracts/src');
      const fileContents = await fs.readFile(contractDirectory + '/lcValidator.hl', 'utf8');
    
      const contractScript = fileContents.toString();
      const compiledScript = Program.new(contractScript).compile(optimize);
      const valHash = compiledScript.validatorHash;
      const valAddr = Address.fromValidatorHash(true, valHash);
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

      if (payload && (payload[0].reference_script_hash === valHash.hex)) {
        const lcVal = {
          lcValAddr: valAddr.toBech32(),
          lcValAdaAmt: payload[0].amount[0].quantity, 
          lcRefTxId: payload[0].tx_hash,
          lcRefTxIdx: payload[0].output_index
        }
      return { props: lcVal }
      }
    } catch (err) {
      console.log('getServerSideProps', err);
    } 
    return { props: undefined };
  }


const Home: NextPage = (props) => {

  const lcValidatorScriptAddress = props.lcValAddr as string;
  const lcValAdaAmt = props.lcValAdaAmt as string;
  const lcValRefTxId = props.lcRefTxId as string;
  const lcValRefTxIdx = props.lcRefTxIdx as string;

  const optimize = false;
  const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
  const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  const threadTokenMPH = process.env.NEXT_PUBLIC_THREAD_TOKEN_MPH as string;
  const threadTokenName = process.env.NEXT_PUBLIC_THREAD_TOKEN_NAME as string;
  const ownerPkh = process.env.NEXT_PUBLIC_OWNER_PKH as string;

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
      throw console.error(payload);
    }

    payload = await resp.json();
    const lovelaceAmount = payload[0].amount[0].quantity;
    const mph = MintingPolicyHash.fromHex(threadTokenMPH);
    const token = hexToBytes(threadTokenName);

    const value = new Value(BigInt(lovelaceAmount), new Assets([
        [mph, [
            [token, BigInt(1)]
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

  const getLCRefUtxo = async () => {

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
    return true;
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

    const info = await fetchLittercoinInfo();
    const datObj = info?.datum;
    const datAda : number = Object.values(datObj?.list[0]) as unknown as number;
    const datLC : number = Object.values(datObj?.list[1]) as unknown as number;
    const newAdaAmount : BigInt = BigInt(datAda) + BigInt(adaQty*1000000);
    const newDatAda = new IntData(newAdaAmount.valueOf());
    const newDatLC = new IntData(BigInt(datLC));
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

    const cborColatUtxo = await walletAPI.getCollateral();
    const colatUtxo = UTxO.fromCbor(hexToBytes(cborColatUtxo[0]));

    const response = await fetch('/api/lcValidator'); 
    const contractScript = await response.text();

    const compiledScript = Program.new(contractScript).compile(optimize);
    //console.log("prettyIR", Program.new(contractScript).prettyIR());
    const valAddr = Address.fromValidatorHash(true, compiledScript.validatorHash);

    // Currently not working in tx builder using changeAddr from wallet
    //const cborChangeAddr = await walletAPI.getChangeAddress();
    //const changeAddr = Address.fromCbor(hexToBytes(cborChangeAddr));
    const changeAddr = Address.fromBech32("addr_test1vq7k907l7e59t52skm8e0ezsnmmc7h4xy30kg2klwc5n8rx5wscj60kfrsr64u3lf3sxnd7a375cscwe4t4jp39euxmqqchlvw");
    
    const tx = new Tx();

    for (const utxo of Utxos) {
        tx.addInput(utxo);
    }

    const valUtxo = await getTTUtxo();
    tx.addInput(valUtxo, valRedeemer);

    const valRefUtxo = await getLCRefUtxo();
    tx.addRefInput(
        valRefUtxo,
        compiledScript
    );

    const newInlineDatum = Datum.inline(newDatum);
    const value = new Value(newAdaAmount.valueOf(), new Assets([
      [MintingPolicyHash.fromHex(threadTokenMPH), [
          [hexToBytes(threadTokenName), BigInt(1)]
      ]]
    ]));

    // send Ada, updated dautm and thread token back to script address
    tx.addOutput(new TxOutput(valAddr, value, newInlineDatum));
    tx.addCollateral(colatUtxo);

    const networkParams = new NetworkParams(
      await fetch("https://d1t0d7c2nekuk0.cloudfront.net/preprod.json")
          .then(response => response.json())
    )

    console.log("tx before final", tx.dump());

    // send any change back to the buyer
    await tx.finalize(networkParams, changeAddr);
    console.log("tx after final", tx.dump());

    console.log("Waiting for wallet signature...");
    const walletResp = await walletAPI.signTx(bytesToHex(tx.toCbor()), true)

    console.log("Verifying signature...");
    const signatures = TxWitnesses.fromCbor(hexToBytes(walletResp)).signatures
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
