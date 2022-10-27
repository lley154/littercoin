
import AddAda from '../components/AddAda';
import BurnLC from '../components/BurnLC';
import Head from 'next/head'
import LittercoinInfo from '../components/LittercoinInfo';
import MintLC from '../components/MintLC';
import MintNFT from '../components/MintNFT';
import type { NextPage } from 'next'
import styles from '../styles/Home.module.css'
import { useState, useEffect } from "react";

import WalletInfo from '../components/WalletInfo';
import {
  Address,  
  TxHash,
  Unit,
  utf8ToHex,
  Blockfrost, 
  C, 
  Constr, 
  Data,
  Json, 
  Lucid, 
  PlutusData, 
  SpendingValidator,
  UTxO,
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

  
  const lcValidatorScript: SpendingValidator = {
    type: "PlutusV2",
    script:
      "5910c85910c50100003323233223233223232323233223232323232323232323232323232323232323232323322323232323232323232323223232323223232232322323253353232323232323335004253355335333573466e1ccdc09aa801110011a8031100100081c81c081c899ab9c4901044c43563800038153353301633301d02833503e335504002733503e335504002735500222002503f503f35007222200135500322222222222200a10391335738921044c43563900038103825335533553353017355003222222222222008103822135002222533500415335333573466e3c008d403088880080f80f44ccd5cd19b8700133702900000301f01e881e91081f881c899ab9c491044c435634000381533553355335333573466e1ccdc09a803110009aa8011100080081c81c0999ab9a3370e66e04d401888008d5400888008cdc100099b83350062200235006220010390381038103913357389201044c43563500038153353301633301d02833503e335504002733503e335504002735500222002503f503f35007222200135500322222222222200a10391335738921044c435637000381038103825335533553353017355003222222222222008103822135002222533500415335333573466e3c008d403088880080f80f44cc07c00401840f48840fc40e44cd5ce249044c43563100038153355335333573466e1ccdc09aa801110009a8031100080081c81c081c899ab9c491044c4356320003815335323235002222222222222533533355303212001503325335333573466e3c03800412011c4d414400454140010841204118d40208888011400c40e44cd5ce2481044c4356330003810381038135533553353500222350022222222222223333500d2504e2504e2504e23335530311200150322350012253355335333573466e3cd400888008d4010880081241204ccd5cd19b873500222001350042200104904810481350520031505100d213500122350012222350092235002222222222222333553033120012235002222253353501822350062232335005233500425335333573466e3c0080041641605400c416081608cd4010816094cd4ccd5cd19b8f0020010590581500310581533500321533500221335002233500223350022335002233041002001205b2335002205b23304100200122205b222335004205b2225335333573466e1c01800c17817454cd4ccd5cd19b8700500205e05d13303f004001105d105d10561533500121056105613350570060051005505200a13263203b3357389201024c660003b130394988854cd400454ccd4d400888880084c98c80f4cd5ce249054c435631300003d2323232153353333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021282411999aab9f500425049233335573e6ae89401494cd54cd4c0c8d5d0a803909a82618268008a82510a99a98199aba15007213504d30020011504b1504a2504a0450440432504704125046250462504625046041213355045007001132632041335738921054c4356313200041135744a00226aae7940044dd5000909931901f19ab9c491054c435631310003e221303d498880044d400488008cccd5cd19b8735573aa00a9000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233503503635742a01866a06a06c6ae85402ccd40d40dcd5d0a805199aa81cbae503835742a012666aa072eb940e0d5d0a80419a81a81f1aba150073335503903f75a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233504975a6ae854008c128d5d09aba2500223263205033573809a0a009c26aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a092eb4d5d0a80118251aba135744a004464c640a066ae701341401384d55cf280089baa001357426ae8940088c98c8130cd5ce02482602509aab9e5001137540026ae854014cd40d5d71aba150043335503903b200135742a006666aa072eb88004d5d0a801181e9aba135744a004464c6409066ae701141201184d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80298169aba135744a00a464c6407466ae700dc0e80e0cccd5cd19b875006480108c84888c00c010dd69aba135573ca01046666ae68cdc3a803a4004464244460040086eb4d5d09aab9e500923333573466e1d4021200023212223001004375a6ae84d55cf280511931901d99ab9c03803b0390380373333573466e1cd55cea804a400046607c6eb4d5d0a8049bad357426ae8940248c98c80e0cd5ce01a81c01b1999ab9a3370e6aae7540352000233332222123333001005004003002375c6ae854034ccd540a1d728139aba1500c375c6ae85402cccd540a1d728139aba135744a016464c6406e66ae700d00dc0d440d84d405524010350543500135573ca00226ea80044d55cea80089baa001135573ca00226ea80044d5d1280089aba25001135573ca00226ea8004c8004d540b888c8c894cd4004409c8854cd4c8c014c024004d4008888800c40a84cc01c018004c8004d540c4894cd4004409c884d40088894cd54cd4cc04800cd5402088800c54cd4cc048008d540208880084cc038004d5402088800440b040b040b44c01c0104cd4c01c48004c00c005200023300250290013200135502c222533500110022213500222330073330080020060010033200135502b22225335001100222135002225335333573466e1c00520000280271333008007006003133300800733502d1233300100800300200600322333573466e1c008004084080c8004d540a4884894cd4ccd5cd19b88001480000880844d40152401035054360015335002135005490103505437002215335333573466e1c00d200002402310021335300612001001337020069001091931901119ab9c00102222233553009120012350012233550290023355300c1200123500122335502c002333500123300a4800000488cc02c0080048cc02800520000013355300912001235001223355029002333500123355300d1200123500122335502d00235500f0010012233355500a00900200123355300d1200123500122335502d00235500e00100133355500500400200122333573466e3c008004074070444888ccd54c010480054088cd54c01c480048d400488cd5409c008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd403c88ccd400c88008008004d40048800448cc004894cd4008409040040848d400488cc028008014018400c4cd409801000d408c004cd54c01c480048d400488c8cd540a000cc004014c8004d540ac894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5409088448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540848844894cd400454080884cd4084c010008cd54c01848004010004c8004d5408088448894cd40044d400c88004884ccd401488008c010008ccd54c01c480040140100044cd4004894cd40088400c4005407048848cc00400c009220100223370000400246666666ae900049406894068940688d406cdd68011280d00a8919118011bac0013200135501b2233335573e0024a032466a03060086ae84008c00cd5d100100b119191999ab9a3370e6aae7540092000233221233001003002300a35742a004600a6ae84d5d1280111931900b19ab9c013016014135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301335742a00466a01a0246ae84d5d1280111931900d99ab9c01801b019135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c8074cd5ce00d00e80d80d00c89aab9d5001137540026ae854008cd4025d71aba135744a004464c6402e66ae7005005c0544d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa03044646666aae7c0089405c8cd4058cd54060c018d55cea80118029aab9e500230043574400602826ae84004488c8c8cccd5cd19b875001480008d4060c014d5d09aab9e500323333573466e1d400920022501823263201433573802202802402226aae7540044dd5000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201433573802202802402202001e26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263201033573801a02001c26aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900719ab9c00b00e00c13754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900b99ab9c01401701501401301201101000f135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8040cd5ce00680800700689aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401a66ae7002803402c0284d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8038cd5ce00580700600580509aab9d5001137540024646666ae68cdc3a800a4004400a46666ae68cdc3a80124000400a464c6401466ae7001c02802001c4d55ce9baa00112200212200149010350543100232632003335738921054c4356313300003498480044488008488488cc00401000c448848cc00400c00848488c00800c44880048848cc00400c008448c8c00400488cc00cc008008005301afd8799f581cb9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682a1581c31940b3430b1b0922b5ed146da240890e2b00129766b819d03388ebaa1581c4c6974746572636f696e20417070726f766564204d65726368616e74014a4c6974746572636f696ea1581c3c71662cfdfeebbaa8363c63d94ddf336c557c6776b789b05c23c385a15820e1eb1d303642182303c75f997eea05b67e19f02425bc4708470cd4eceb4391a801ff0001",
  };
  

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
    const lucid = await Lucid.new(
      
      new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", api_key),
      "Preview",
    );

    
    const lcValidatorScriptAddress: Address = lucid.utils.validatorToAddress(
      lcValidatorScript,
    );
    

    //const lcValidatorScriptAddress: string = "addr_test1wq7rutl3crvwswmn6vn7fjuxmd5spmnek6gell3k6ayjt0ch8vwd0";
    const utxo = await lucid.utxosAt(lcValidatorScriptAddress);
    
    // The threadtoken
    const threadToken = "3c71662cfdfeebbaa8363c63d94ddf336c557c6776b789b05c23c385e1eb1d303642182303c75f997eea05b67e19f02425bc4708470cd4eceb4391a8";
    
    // Iterate through the list of utxos
    for(let i=0; i<utxo.length; i++){

      // Find the utxo that has the thread token
      if (Object.keys(utxo[i].assets).includes(threadToken)) {

        // If found, then convert the CBOR represntation to PlutusData type
        if (utxo[i].datum != undefined) {
          const _datum : PlutusData = Data.from(utxo[i].datum as string);
          //console.log("datum found", _datum);
          return {datum: _datum, address: lcValidatorScriptAddress, utxo: utxo[i]};
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

    const address = params[0];
    const lcQty = params[1];
    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const lucid = await Lucid.new(
      new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", api_key),
      "Preview",
    );
    
    //const lcValidatorScriptAddress: string = "addr_test1wq7rutl3crvwswmn6vn7fjuxmd5spmnek6gell3k6ayjt0ch8vwd0";

    const lcValidatorScriptAddress: Address = lucid.utils.validatorToAddress(
      lcValidatorScript,
    );

    const threadToken = "3c71662cfdfeebbaa8363c63d94ddf336c557c6776b789b05c23c385e1eb1d303642182303c75f997eea05b67e19f02425bc4708470cd4eceb4391a8";
    const _info = await fetchLittercoinInfo();
    const _datum = _info?.datum;
    const oldDatum = Data.to(_datum as PlutusData);

    lucid.selectWallet(API);
    const adminAddr = await lucid.wallet.address();
    const oldAdaAmount = lcInfo.adaAmount;
    const oldLCAmount : number = lcInfo.lcAmount;
    const newLCAmount : number = Number(oldLCAmount) + Number(lcQty);
    const newDatum = Data.to(new Constr(0, [BigInt(oldAdaAmount), BigInt(newLCAmount)]));
    const mintRedeemer = Data.to(new Constr(0, [new Constr(1, []),BigInt(oldAdaAmount),BigInt(0)]));
    const validatorRedeemer = Data.to(new Constr(0, [BigInt(lcQty)]));
    const lcTokenName = "Littercoin";
    const policyId = "5572d1a6d077c47a9be4fe99d444ac55544dcb2965cadb420b95dbf8";
    const lcUnit: Unit = policyId + utf8ToHex(lcTokenName);
    const lcMintAddress : string = "addr_test1wp2h95dx6pmug75munlfn4zy4324gnwt99ju4k6zpw2ah7q8hy4le"; // lc minting policy address
  
    const metaData : Json = {
        "version": "1.0",
        "cd6181a3286e9c0bc27271ef36ae7d400f8e4ffeef174e32f7f53223": {
          "Littercoin": {
            "files": [
              {
                "src": "ipfs://QmT3rYtkkw4wFBP5SfxENAfDY9NuYoZAz2HVng4cQqnVZe",
                "name": "Littercoin",
                "mediaType": "image/png",
                "url": "https://openlittermap.com/"
              }
            ],
            "name": "Open Litter Map",
            "mediaType": "image/png",
            "description": "Help create the world's most advanced open database on litter",
            "image": "ipfs/QmT3rYtkkw4wFBP5SfxENAfDY9NuYoZAz2HVng4cQqnVZe"
          }
        }
      };
   
    // Validator UTXO reference script 
    const referenceLCValidatorUtxo = (await lucid.utxosAt(lcValidatorScriptAddress)).find(
      (utxo) => Boolean(utxo.scriptRef),
    );
    if (!referenceLCValidatorUtxo) throw new Error("LC Validator Reference script not found");
  
    // Validator UTXO spending
    const lcValidatorUtxo = (await lucid.utxosAt(lcValidatorScriptAddress)).find((utxo) =>
      utxo.datum === oldDatum && !utxo.scriptRef
    );
    if (!lcValidatorUtxo) throw new Error("LC Validator Spending script utxo not found");

    // Littercoin minting UTXO reference script
   const referenceLCMintUtxo = (await lucid.utxosAt(lcMintAddress)).find(
     (utxo) => Boolean(utxo.scriptRef),
   );
   if (!referenceLCMintUtxo) throw new Error("LC Minting Reference script not found");

    const tx = await lucid
      .newTx()
      .readFrom([referenceLCValidatorUtxo]) // plutusV2 validator reference script from reference utxo
      .collectFrom([lcValidatorUtxo], validatorRedeemer)
      .mintAssets({ [lcUnit]: lcQty }, mintRedeemer) // mint littercoin token
      .readFrom([referenceLCMintUtxo]) // plutusV2 minting reference script from reference utxo
      .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(oldAdaAmount), [threadToken]: BigInt(1), })
      .payToAddress(address, { [lcUnit]: lcQty }) // send littercoin token to user wallet address
      .addSigner(adminAddr)
      .attachMetadata(721, metaData)
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
   } 

  const burnLC = async (lcQty : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const lucid = await Lucid.new(
      new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", api_key),
      "Preview",
    );
    const lcValidatorScriptAddress: string = "addr_test1wq7rutl3crvwswmn6vn7fjuxmd5spmnek6gell3k6ayjt0ch8vwd0";
    const threadToken = "3c71662cfdfeebbaa8363c63d94ddf336c557c6776b789b05c23c385e1eb1d303642182303c75f997eea05b67e19f02425bc4708470cd4eceb4391a8";

    const merchantToken = "31940b3430b1b0922b5ed146da240890e2b00129766b819d03388eba4c6974746572636f696e20417070726f766564204d65726368616e74";
    const _info = await fetchLittercoinInfo();
    const _datum = _info?.datum;
    const oldDatum = Data.to(_datum as PlutusData);

    lucid.selectWallet(API);
    const merchAddr = await lucid.wallet.address()
    const oldAdaAmount = lcInfo.adaAmount;
    const oldLCAmount : number = lcInfo.lcAmount;
    const newLCAmount : number = Number(oldLCAmount) - Number(lcQty);
    const ratio : number = lcInfo.adaAmount / lcInfo.lcAmount;
    const withdrawAda : number = Number(ratio) * Number(lcQty);
    const newAdaAmount : number = Number(oldAdaAmount) - Number(withdrawAda);
    const newDatum = Data.to(new Constr(0, [BigInt(newAdaAmount), BigInt(newLCAmount)]));
    const mintRedeemer = Data.to(new Constr(0, [new Constr(0, []),BigInt(newAdaAmount),BigInt(withdrawAda)]));
    const validatorRedeemer = Data.to(new Constr(1, [BigInt(lcQty)]));
    const lcTokenName = "Littercoin";
    const policyId = "5572d1a6d077c47a9be4fe99d444ac55544dcb2965cadb420b95dbf8";
    const lcUnit: Unit = policyId + utf8ToHex(lcTokenName);
    const lcMintAddress : string = "addr_test1wp2h95dx6pmug75munlfn4zy4324gnwt99ju4k6zpw2ah7q8hy4le"; // lc minting policy address
  
    // Validator UTXO reference script
    const referenceLCValidatorUtxo = (await lucid.utxosAt(lcValidatorScriptAddress)).find(
      (utxo) => Boolean(utxo.scriptRef),
    );
    if (!referenceLCValidatorUtxo) throw new Error("LC Validator Reference script not found");

    // Validator UTXO spending
    const lcValidatorUtxo = (await lucid.utxosAt(lcValidatorScriptAddress)).find((utxo) =>
      utxo.datum === oldDatum && !utxo.scriptRef
    );
    if (!lcValidatorUtxo) throw new Error("LC Validator Spending script utxo not found");

    // Littercoin minting UTXO reference script
    const referenceLCMintUtxo = (await lucid.utxosAt(lcMintAddress)).find(
      (utxo) => Boolean(utxo.scriptRef),
    );
    if (!referenceLCMintUtxo) throw new Error("LC Minting Reference script not found");

    const tx = await lucid
      .newTx()
      .readFrom([referenceLCValidatorUtxo]) // plutusV2 validator reference script from reference utxo
      .collectFrom([lcValidatorUtxo], validatorRedeemer) // spending utxos which includes threadtoken
      .mintAssets({ [lcUnit]: -(BigInt(lcQty)) }, mintRedeemer) // burn littercoin token
      .readFrom([referenceLCMintUtxo]) // plutusV2 minting reference script from reference utxo
      .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(newAdaAmount), [threadToken] : BigInt(1), })
      .payToAddress(merchAddr, { ["lovelace"]: BigInt(withdrawAda) , [merchantToken] : BigInt(1) }) // send redeemed Ada to merchant address
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  } 


  const mintNFT = async (nftAddress : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const lucid = await Lucid.new(
      new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", api_key),
      "Preview",
    );

    lucid.selectWallet(API);

    /*

    const mintingPolicy: MintingPolicy = {
      type: "PlutusV2",
      script:
        "5909c05909bd010000332323322323232332232323232323232323232323232323322323232323232323232322335502322323232232325335330083333573466e1cd55cea80324000466644424666002008006004603c6ae854018dd69aba15005375a6ae84d5d1280291931901019ab9c02102001e3333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4074078d5d0a80619a80e80f1aba1500b33501d01f35742a014666aa042eb94080d5d0a804999aa810bae502035742a01066a03a0506ae85401cccd540840a5d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40cdd69aba150023034357426ae8940088c98c80d8cd5ce01b81b01a09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a819bad35742a00460686ae84d5d1280111931901b19ab9c037036034135573ca00226ea8004d5d09aba2500223263203233573806606406026aae7940044dd50009aba1500533501d75c6ae854010ccd540840948004d5d0a801999aa810bae200135742a004604e6ae84d5d1280111931901719ab9c02f02e02c135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a004602e6ae84d5d1280111931901019ab9c02102001e101f13263201f335738921035054350001f135573ca00226ea80044d5d1280089aab9e50011375400244646a006444a66a006266048660149201054e46545031005335301f3009500410272213500222253350041330293332001502a002301000c3332001502b001480088840b8cc029241054e4654503200335502932235002222222222222533533355301612001321233001225335002210031001002502025335333573466e3c0380040dc0d84d408800454084010840dc40d54010c8004d401c880044cc029241054e46545033005335301f3009500410272213500222253350041330293332001502a002301000c3332001502b001480048840b84d400488008c8004d5408888448894cd40044d400c88004884ccd401488008c010008ccd54c01c480040140100048d4004888888888888020894cd400440804cd5ce00100f91a800910010919118011bac0013200135501f2233335573e0024a036466a03460086ae84008c00cd5d100100a119191999ab9a3370e6aae7540092000233221233001003002300c35742a004600a6ae84d5d1280111931900a19ab9c015014012135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301535742a00466a01a0286ae84d5d1280111931900c99ab9c01a019017135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c806ccd5ce00e00d80c80c00b89aab9d5001137540026ae854008cd4025d71aba135744a004464c6402a66ae7005805404c4d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa03844646666aae7c008940648cd4060cc8848cc00400c008c018d55cea80118029aab9e500230043574400602426ae84004488c8c8cccd5cd19b875001480008d401cc014d5d09aab9e500323333573466e1d400920022500723263201233573802602402001e26aae7540044dd50008909118010018891000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201033573802202001c01a01801626aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263200c33573801a01801426aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900519ab9c00b00a00813754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900999ab9c01401301101000f00e00d00c00b135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8030cd5ce00680600500489aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401266ae7002802401c0184d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8028cd5ce00580500400380309aab9d5001137540024646666ae68cdc3a800a4004402046666ae68cdc3a801240004020464c6400c66ae7001c01801000c4d55ce9baa00149848005241035054310023300250050013200135500a222533500110022213500222330073330080020060010033200135500922225335001100222135002225335333573466e1c005200000f00e133300800700600313330080073350091233300100800300200600311220021221223300100400322533500210011005122333573466e3c008004014010488ccd5cd19b8700200100400312200212200111223002001112323001001223300330020020013351223300248811c4c6974746572636f696e20417070726f766564204d65726368616e740048811cb9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c276820022123300100300220011",
    };
    const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);
    const name: string = "Littercoin Approved Merchant";
    const unit: Unit = policyId + utf8ToHex(name);
    const addr : string = lucid.utils.getAddressDetails(nftAddress.address).address.bech32
    */

    const policyId = "31940b3430b1b0922b5ed146da240890e2b00129766b819d03388eba";
    const name: string = "Littercoin Approved Merchant";
    const unit: Unit = policyId + utf8ToHex(name);
    const addr : string = "addr_test1wqcegze5xzcmpy3ttmg5dk3ypzgw9vqp99mxhqvaqvugawsv6qrzk"; // nft minting policy address
    const qty = BigInt(1);  // only 1 NFT token
    const adminAddr = await lucid.wallet.address();
    const mintRedeemer = Data.to(new Constr(0, [new Constr(1, []),BigInt(0),BigInt(0)]));
    const referenceScriptUtxo = (await lucid.utxosAt(addr)).find(
      (utxo) => Boolean(utxo.scriptRef),
    );
    if (!referenceScriptUtxo) throw new Error("Reference script not found");

    const tx = await lucid
      .newTx()
      .mintAssets({ [unit]: qty }, mintRedeemer)
      .readFrom([referenceScriptUtxo]) // spending utxo by reading plutusV2 from reference utxo
      .payToAddress(nftAddress, { [unit]: qty }) // send userToken to user wallet address
      .addSigner(adminAddr)
      .complete();
  
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("txHash", txHash);
    setTx({ txId: txHash });
    return txHash;
  }   


  const addAda = async (adaQty : any) : Promise<TxHash> => {

    const api_key : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
    const lucid = await Lucid.new(
      new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", api_key),
      "Preview",
    );
    
    //const lcValidatorScriptAddress: string = "addr_test1wq7rutl3crvwswmn6vn7fjuxmd5spmnek6gell3k6ayjt0ch8vwd0";
    const lcValidatorScriptAddress: Address = lucid.utils.validatorToAddress(
      lcValidatorScript,
    );

    const threadToken = "3c71662cfdfeebbaa8363c63d94ddf336c557c6776b789b05c23c385e1eb1d303642182303c75f997eea05b67e19f02425bc4708470cd4eceb4391a8";  

    const _info = await fetchLittercoinInfo();
    const _datum = _info?.datum;
    const oldDatum = Data.to(_datum as PlutusData);

    lucid.selectWallet(API);
    const oldAdaAmount = lcInfo.adaAmount;
    const oldLCAmount : number = lcInfo.lcAmount;
    const newAdaAmount : number = Number(oldAdaAmount) + Number(adaQty);
    const newDatum = Data.to(new Constr(0, [BigInt(newAdaAmount), BigInt(oldLCAmount)]));
    const validatorRedeemer = Data.to(new Constr(2, [BigInt(adaQty)]));

    //const lcValidatorScriptAddress: string = "addr_test1wq7rutl3crvwswmn6vn7fjuxmd5spmnek6gell3k6ayjt0ch8vwd0";
    const utxo = await lucid.utxosAt(lcValidatorScriptAddress);

    var spendUtxo;
    // Iterate through the list of utxos
    for(let i=0; i<utxo.length; i++){

      // Find the utxo that has the thread token
      if (Object.keys(utxo[i].assets).includes(threadToken)) {
        spendUtxo = utxo[i];

      }
    }


    // Validator UTXO reference script
    const referenceLCValidatorUtxo = (await lucid.utxosAt(lcValidatorScriptAddress)).find(
      (utxo) => Boolean(utxo.scriptRef),
    );
    if (!referenceLCValidatorUtxo) throw new Error("LC Validator Reference script not found");

    // Validator UTXO spending
    const lcValidatorUtxo = (await lucid.utxosAt(lcValidatorScriptAddress)).find((utxo) =>
      utxo.datum === oldDatum && !utxo.scriptRef
    );
    if (!lcValidatorUtxo) throw new Error("LC Validator Spending script utxo not found");

    /*
    const tx = await lucid
      .newTx()
      .readFrom([referenceLCValidatorUtxo]) // plutusV2 validator reference script from reference utxo
      .collectFrom([lcValidatorUtxo], validatorRedeemer) // spending utxos which includes threadtoken
      .payToContract(lcValidatorScriptAddress, { inline: newDatum }, { ["lovelace"] : BigInt(newAdaAmount), [threadToken] : BigInt(1), })
      .complete();
    */
    var spendUtxos : UTxO[] = []; 
    if (spendUtxo) {
      spendUtxos = [spendUtxo];
    } else {
      spendUtxo = [];
    }

    const tx = await lucid
    .newTx()
    .collectFrom(spendUtxos, validatorRedeemer)
    .attachSpendingValidator(lcValidatorScript)
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
        <title>Littercoin Preview Testnet</title>
        <meta name="description" content="Littercoin web tools page" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <h3 className={styles.title}>
          Littercoin Preview Testnet
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
            <p>TxId &nbsp;&nbsp;<a href={"https://preview.cexplorer.io/tx/" + tx.txId} target="_blank" rel="noopener noreferrer" >{tx.txId}</a></p>
            <p>Please wait until the transaction is confirmed on the blockchain and reload this page before doing another transaction</p>
          </div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><AddAda onAddAda={addAda}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintLC onMintLC={mintLC}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><BurnLC onBurnLC={burnLC}/></div>}
          {walletIsEnabled && !tx.txId && <div className={styles.border}><MintNFT onMintNFT={mintNFT}/></div>}

      </main>

      <footer className={styles.footer}>

      </footer>
    </div>
  )
}

export default Home
