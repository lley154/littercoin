import type { NextApiRequest, NextApiResponse } from 'next'
import { Bip32PrivateKey } from '@stricahq/bip32ed25519';
import { mnemonicToEntropy } from 'bip39';
import { Buffer } from "buffer";
import { blake2b } from "blakejs";
import {
    bytesToHex, 
    ByteArrayData,
    hexToBytes, 
    PubKeyHash,
    Signature,
    TxWitnesses,
    Tx } from "@hyperionbt/helios";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse ) {

  const orderAPIKey = process.env.NEXT_PUBLIC_ORDER_API_KEY as string;

  if (req.method == 'POST') {

    // Check for basic auth header
    if (!req.headers.authorization || req.headers.authorization.indexOf('Basic ') === -1) {
        throw { status: 401, message: 'Missing Authorization Header' };
    }

    // Verify auth credentials
    const apiKey = req.headers.authorization.split(' ')[1];
    if (orderAPIKey != apiKey) {
        throw { status: 401, message: 'Invalid Authentication Credentials' };
    }

    // TODO - API call to open littermap

    const hash28 = (data: any) => {
      const hash = blake2b(data, undefined, 28);
      return Buffer.from(hash);
    };
    
    const hash32 = (data: any) => {
      const hash = blake2b(data, undefined, 32);
      return Buffer.from(hash);
    };
    
    function harden(num: number) {
      return 0x80000000 + num;
    }

    const entropy = mnemonicToEntropy(
      [ "witness", "pipe", "egg", "awake", "hood", "false", "fury", "announce", "one", "wool", "diagram", "weird", "phone", "treat", "bacon" ].join(' ')
    );
    
    const buffer = Buffer.from(entropy, 'hex');
    const rootKey = await Bip32PrivateKey.fromEntropy(buffer);


    const accountKey = rootKey
      .derive(harden(1852)) // purpose
      .derive(harden(1815)) // coin type
      .derive(harden(0)); // account #0
      
    const utxoPrvKey = accountKey
      .derive(0) // external
      .derive(0)
      .toPrivateKey();

    const utxoPubKey = accountKey
      .derive(0) // external
      .derive(0)
      .toBip32PublicKey();
      
    const stakeKey = accountKey
      .derive(2) // chimeric
      .derive(0)
      .toBip32PublicKey();
    
    const txCbor = req.body.txCbor;
    const tx = Tx.fromCbor(hexToBytes(txCbor));
    const txBodyCbor = bytesToHex((tx.body).toCbor());
    const txHash = hash32(txBodyCbor);
    const pubKey = (hash28(utxoPubKey.toBytes().slice(0,32)));


    const signature = new Signature(utxoPubKey.toBytes().subarray(0, 32).toString('hex'),
                                    utxoPrvKey.sign(txHash));

    const sigCbor = bytesToHex(signature.toCbor());

    res.status(200).json(sigCbor);
  }
  else {
    res.status(400);
  }
}



