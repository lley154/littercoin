/** @typedef {import('./helios.js').Tx} Tx */
import { Assets, ConstrData, Datum, TxId, MintingPolicyHash, NetworkParams, Value, TxOutput, UTxO, hexToBytes } from './helios.js';

const BLOCKFROST_API_KEY = "preprodxg6GaNVZoHWUfQd7HQcgUg8epWhE1aMi";

const NETWORK_PARAMS_URL = "https://d1t0d7c2nekuk0.cloudfront.net/preprod.json";

/**
 * @param {{}} obj 
 * @returns 
 */
function blockFrostAmountToValue(obj) {
    let value = new Value();

    for (let item of obj) {
        let qty = BigInt(item.quantity);

        if (item.unit == "lovelace") {
            value = value.add(new Value(qty));
        } else {
            let policyID = item.unit.substring(0, 56);
            let mph = MintingPolicyHash.fromHex(policyID);

            let token = hexToBytes(item.unit.substring(56));

            value = value.add(new Value(0n, new Assets([
                [mph, [
                    [token, qty]
                ]]
            ])));
        }
    }

    return value;
}

/**
 * PreprodNetwork uses BlockFrost as its source
 */
export default class PreprodNetwork {
    #params;

    /**
     * @param {NetworkParams} params 
     */
    constructor(params) {
        this.#params = params;
    }

    /**
     * @returns {Promise<PreprodNetwork>}
     */
    static async new() {
        const networkParams = new NetworkParams(await fetch(NETWORK_PARAMS_URL).then(response => response.json()));

        return new PreprodNetwork(networkParams);
    }

    get name() {
        return "preprod";
    }

    get params() {
        return this.#params;
    }

    get fetchConfig() {
        return {
            headers: {
                project_id: BLOCKFROST_API_KEY
            }
        };
    }

    isTestnet() {
        return true;
    }

    /**
     * @param {UTxO} utxo 
     * @returns {Promise<boolean>}
     */
    async hasUtxo(utxo) {
        const txId = utxo.txId;

        const url = `https://cardano-${this.name}.blockfrost.io/api/v0/txs/${txId.hex}/utxos`;

        const response = await fetch(url, this.fetchConfig);

        return response.ok;
    }

    /**
     * @param {Address} addr 
     * @returns {Promise<UTxO[]>}
     */
    async getUtxos(addr) {
        const url = `https://cardano-${this.name}.blockfrost.io/api/v0/addresses/${addr.toBech32()}/utxos?order=asc`;

        /** @type {{}[]} */
        let all = await fetch(url, this.fetchConfig).then(response => {
            return response.json()
        });

        if (all?.status_code > 299) {
            all = [];
        }

        return all.map(obj => {
            return new UTxO(
                TxId.fromHex(obj.tx_hash),
                BigInt(obj.output_index),
                new TxOutput(
                    addr,
                    blockFrostAmountToValue(obj.amount),
                    Datum.inline(ConstrData.fromCbor(hexToBytes(obj.inline_datum)))
                )
            );
        });
    }

    /**
     * @param {Tx} tx 
     * @returns {Promise<string>}
     */
    submitTx(tx) {
        const data = new Uint8Array(tx.toCbor());
        const url = `https://cardano-${this.name}.blockfrost.io/api/v0/tx/submit`;

        return new Promise((resolve, reject) => {
            const req = new XMLHttpRequest();
            req.onload = (_e) => {
                if (req.status == 200) {
                    resolve(req.responseText);
                } else {
                    reject(new Error(req.responseText));
                }
            }

            req.onerror = (e) => {
                reject(e);
            }

            req.open("POST", url, false);

            req.setRequestHeader("content-type", "application/cbor");
            req.setRequestHeader("project_id", BLOCKFROST_API_KEY);
            
            req.send(data);
        });   
    }
}
