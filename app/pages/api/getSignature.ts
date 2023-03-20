import type { NextApiRequest, NextApiResponse } from 'next'
import { Bip32PrivateKey } from '@stricahq/bip32ed25519';
import { Buffer } from "buffer";
import { blake2b } from "blakejs";
import {
    bytesToHex, 
    hexToBytes, 
    Signature,
    Tx } from "@hyperionbt/helios";

import axios from 'axios';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse ) {

    const submitTx = async (tx: Tx) : Promise<string> => {

      const payload = new Uint8Array(tx.toCbor());
      const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
      const blockfrostUrl = blockfrostAPI + "/tx/submit";
      const apiKey : string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;
  
      try {
        let res = await axios({
              url: blockfrostUrl,
              data: payload,
              method: 'post',
              timeout: 8000,
              headers: {
                  'Content-Type': 'application/cbor',
                  'project_id': apiKey
              }
          })
          if(res.status == 200){
              return res.data;
          } else {
            console.error("submitTx API Blockfrost Error: ", res.data);
            throw res.data;
          }   
      }
      catch (err) {
          console.error("submitTx API Failed: ", err);
          throw err;
      }
    }

    const hash32 = (data: any) => {
        const hash = blake2b(data, undefined, 32);
        return Buffer.from(hash);
    };
        
    function harden(num: number) {
        return 0x80000000 + num;
    }
    
    try {
        const rootKeyHex : string = process.env.NEXT_PUBLIC_ROOT_KEY as string;
        const buffer = Buffer.from(rootKeyHex, 'hex');
        const rootKey = new Bip32PrivateKey(buffer);

        const accountKey = rootKey
        .derive(harden(1852)) // purpose
        .derive(harden(1815)) // coin type
        .derive(harden(0)); // account #0
        
        const addrPrvKey = accountKey
        .derive(0) // external
        .derive(0)
        .toPrivateKey();

        const addrPubKey = accountKey
        .derive(0) // external
        .derive(0)
        .toBip32PublicKey();
        
        const txCbor = req.body;
        const tx = Tx.fromCbor(hexToBytes(txCbor));
      

        // PUT YOUR BACK-END VALIDATION LOGIC HERE

        const txBodyCbor = bytesToHex((tx.body).toCbor());
        const txBody = Buffer.from(txBodyCbor, 'hex');
        const txHash = hash32(txBody);

        const pubKeyArray = [...addrPubKey.toBytes().subarray(0, 32)];
        const signatureArray = [...addrPrvKey.sign(txHash)];

        const signature = new Signature(pubKeyArray,
                                        signatureArray);

        tx.addSignature(signature);
        const txId = await submitTx(tx);
        console.log("txId", txId);
        res.status(200).send(txId);
    }
    catch (err) {
        res.status(500).json("getSignature API error: " + err);
    }
}
