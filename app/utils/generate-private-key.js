import { mnemonicToEntropy } from 'bip39';
import { bytesToHex} from '@hyperionbt/helios';
import pkg from '@stricahq/bip32ed25519';
const { Bip32PrivateKey } = pkg;


// Usage: node ./utils/generate-private-key.js

// PLEASE NOTE: DO NOT STORE THE REAL PASS PHRASE IN APPLICATION CODE...THIS IS DONE FOR EXAMPLE PURPOSES ONLY
const entropy = mnemonicToEntropy(
    [ "witness", "pipe", "egg", "awake", "hood", "false", "fury", "announce", "one", "wool", "diagram", "weird", "phone", "treat", "bacon" ].join(' ')
    );

const buffer = Buffer.from(entropy, 'hex');
const rootKey = await Bip32PrivateKey.fromEntropy(buffer);
console.log("rootKey: ", rootKey);

const key = [...rootKey.toBytes()];
const keyStore = bytesToHex(key);
console.log("rootKeyHex", keyStore);