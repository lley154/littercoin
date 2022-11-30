
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

  /*
  const lcValidatorScript: SpendingValidator = {
    type: "PlutusV2",
    script:
      "591085591082010000332323322323322323232323322323232323232323232323232323232323232323232323232323322323232323232323223232323223232232322323253353232323232323335004253355335333573466e1ccdc09aa801110011a8031100100081d01c881d099ab9c4901044c43563800039153353301633301a02933503f335504102833503f3355041028355002220025040504035007222200135500322222222222200a103a1335738921044c43563900039103925335533553353025355003222222222222008103922135002222533500415335333573466e3c008d403088880080fc0f84ccd5cd19b8700133702900000301f81f081f110820081d099ab9c491044c435634000391533553355335333573466e1ccdc09a803110009aa8011100080081d01c8999ab9a3370e66e04d401888008d5400888008cdc100099b833500622002350062200103a0391039103a13357389201044c43563500039153353301633301a02933503f335504102833503f3355041028355002220025040504035007222200135500322222222222200a103a1335738921044c435637000391039103925335533553353025355003222222222222008103922135002222533500415335333573466e3c008d403088880080fc0f84cc07000401840f888410040e84cd5ce249044c43563100039153355335333573466e1ccdc09aa801110009a8031100080081d01c881d099ab9c491044c4356320003915335323235002222222222222533533355302f12001503125335333573466e3c0380041241204d41480045414401084124411cd40208888011400c40e84cd5ce2481044c4356330003910391039135533553353500222350022222222222223333500d2504f2504f2504f233355302e1200150302350012253355335333573466e3cd400888008d4010880081281244ccd5cd19b873500222001350042200104a04910491350530031505200d213500122350012222350092235002222222222222333553030120012235002222253353501822350062232335005233500425335333573466e3c0080041681645400c416481648cd4010816494cd4ccd5cd19b8f00200105a0591500310591533500321533500221335002233500223350022335002233045002001205c2335002205c23304500200122205c222335004205c2225335333573466e1c01800c17c17854cd4ccd5cd19b8700500205f05e13303c004001105e105e10571533500121057105713350580060051005505300a13263203c3357389201024c660003c1303a4988854cd400454ccd4d400888880084c98c80f8cd5ce249054c435631300003e2323232153353333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021282491999aab9f50042504a233335573e6ae89401494cd54cd4c0ccd5d0a803909a82698270008a82590a99a981a1aba15007213504e30020011504c1504b2504b0460450442504804225047250472504725047042213355046007001132632042335738921054c4356313200042135744a00226aae7940044dd5000909931901f99ab9c491054c435631310003f221303e498880044d400488008cccd5cd19b8735573aa00a9000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233503603735742a01866a06c06e6ae85402ccd40d80e0d5d0a805199aa81d3ae503935742a012666aa074eb940e4d5d0a80419a81b01f9aba150073335503a04075a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233504a75a6ae854008c12cd5d09aba2500223263205133573809c0a209e26aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a094eb4d5d0a80118259aba135744a004464c640a266ae7013814413c4d55cf280089baa001357426ae8940088c98c8134cd5ce02502682589aab9e5001137540026ae854014cd40d9d71aba150043335503a03c200135742a006666aa074eb88004d5d0a801181f1aba135744a004464c6409266ae7011812411c4d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80298171aba135744a00a464c6407666ae700e00ec0e4cccd5cd19b875006480108c84888c00c010dd69aba135573ca01046666ae68cdc3a803a4004464244460040086eb4d5d09aab9e500923333573466e1d4021200023212223001004375a6ae84d55cf280511931901e19ab9c03903c03a0390383333573466e1cd55cea804a400046607e6eb4d5d0a8049bad357426ae8940248c98c80e4cd5ce01b01c81b9999ab9a3370e6aae7540352000233332222123333001005004003002375c6ae854034ccd540a5d728141aba1500c375c6ae85402cccd540a5d728141aba135744a016464c6407066ae700d40e00d840dc4c98c80dccd5ce24810350543500037135573ca00226ea80044d55cea80089baa001135573ca00226ea80044d5d1280089aba25001135573ca00226ea8004c8004d540bc8894cd400440988854cd4c8ccd54c04448004d402540208d400488ccd54c05048004d4031402c8d400488ccd40048cc0392000001223300f00200123300e00148000004cc02c004014d4008888800c40a44cc01401000488ccd5cd19b8700200102502411233001225335002100110240231233500c223335003220020020013500122001222335530091200123500122335502d0023355300c12001235001223355030002333500123300a4800000488cc02c0080048cc02800520000013300400200122335530071200123500122335502b002333500123355300b1200123500122335502f00235500d0010012233355500800e00200123355300b1200123500122335502f00235500c001001333555003009002001111222333553004120015026335530071200123500122335502b00235500900133355300412001223500222533533355300c1200132335010223335003220020020013500122001123300122533500210281001025235001223300a0020050061003133502a004003502700133553007120012350012232335502c00330010053200135502f225335001135500a003221350022253353300c002008112223300200a004130060030023200135502822112225335001100222133005002333553007120010050040011121222300300411212223001004320013550252211225335001150242213350253004002335530061200100400132001355024221122253350011350032200122133350052200230040023335530071200100500400122333573466e3c0080040680644cd4004894cd40088400c4005407c48848cc00400c0088cc0094070004c8004d5407c8894cd40044008884d400888cc01cccc02000801800400cc8004d5407888894cd40044008884d4008894cd4ccd5cd19b870014800006c0684ccc02001c01800c4ccc02001ccd408048ccc00402000c00801800d22100223370000400246666666ae900049406894068940688d406cdd68011280d00a8919118011bac0013200135501b2233335573e0024a032466a03060086ae84008c00cd5d100100b119191999ab9a3370e6aae7540092000233221233001003002300a35742a004600a6ae84d5d1280111931900b19ab9c013016014135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301335742a00466a01a0246ae84d5d1280111931900d99ab9c01801b019135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c8074cd5ce00d00e80d80d00c89aab9d5001137540026ae854008cd4025d71aba135744a004464c6402e66ae7005005c0544d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa03044646666aae7c0089405c8cd4058cd54060c018d55cea80118029aab9e500230043574400602826ae84004488c8c8cccd5cd19b875001480008d4060c014d5d09aab9e500323333573466e1d400920022501823263201433573802202802402226aae7540044dd5000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201433573802202802402202001e26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263201033573801a02001c26aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900719ab9c00b00e00c13754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900b99ab9c01401701501401301201101000f135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8040cd5ce00680800700689aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401a66ae7002803402c0284d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8038cd5ce00580700600580509aab9d5001137540024646666ae68cdc3a800a4004400a46666ae68cdc3a80124000400a464c6401466ae7001c02802001c4d55ce9baa00112200212200149010350543100232632003335738921054c4356313300003498480044488008488488cc00401000c448848cc00400c00848488c00800c44880048848cc00400c008448c8c00400488cc00cc008008005301afd8799f581cb9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682a1581c31940b3430b1b0922b5ed146da240890e2b00129766b819d03388ebaa1581c4c6974746572636f696e20417070726f766564204d65726368616e74014a4c6974746572636f696ea1581c3c71662cfdfeebbaa8363c63d94ddf336c557c6776b789b05c23c385a15820caf74d41a0b16a6d2bf601796c2b73912538cd2faaccae1d9c6da7d405873fd301ff0001",
  };
  */

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
    const network = process.env.NEXT_PUBLIC_NETWORK as string;

    const lucid = await Lucid.new(
      
      new Blockfrost(blockfrost_url, api_key),
      "Preprod",
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
