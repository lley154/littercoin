import type { NextApiRequest, NextApiResponse } from 'next'
import { Bip32PrivateKey } from '@stricahq/bip32ed25519';
import { mnemonicToEntropy } from 'bip39';
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

    const blockfrostAPI = process.env.NEXT_PUBLIC_BLOCKFROST_API as string;
    const apiKey: string = process.env.NEXT_PUBLIC_BLOCKFROST_API_KEY as string;

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

    const hash32 = (data: any) => {
        const hash = blake2b(data, undefined, 32);
        return Buffer.from(hash);
    };
        
    function harden(num: number) {
        return 0x80000000 + num;
    }

    // PLEASE NOTE: DO NOT STORE THE REAL PASS PHRASE IN APPLICATION CODE...THIS IS DONE FOR EXAMPLE PURPOSES ONLY
    const entropy = mnemonicToEntropy(
        [ "witness", "pipe", "egg", "awake", "hood", "false", "fury", "announce", "one", "wool", "diagram", "weird", "phone", "treat", "bacon" ].join(' ')
        );
    
    try {
        const buffer = Buffer.from(entropy, 'hex');
        const rootKey = await Bip32PrivateKey.fromEntropy(buffer);

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
        
        const txCbor = req.body.txCbor;
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
        res.status(200).json(txId);
    }
    catch (err) {
        res.status(500).json('getSignature error: ' + err);
    }
}
