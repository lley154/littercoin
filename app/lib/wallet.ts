/** @typedef {import(@hyperionbt/helios).Tx} Tx */
/** @typedef {import(@hyperionbt/helios).Signature}  Signature */
/** @typedef {import(@hyperionbt/helios).PubKeyHash} PubKeyHash */
import { Address, bytesToHex, hexToBytes, TxWitnesses, UTxO, Value } from "@hyperionbt/helios";

/**
 * Sync cached version of Wallet
 */
export class WalletState {
    #addresses;
    #changeAddressIndex;
    #utxos;

    /**
     * @param {Address[]} addresses
     * @param {number} changeAddressIndex
     * @param {UTxO[]} utxos
     */
    constructor(addresses: any, changeAddressIndex: any, utxos : any) {
        this.#addresses = addresses;
        this.#changeAddressIndex = changeAddressIndex;
        this.#utxos = utxos;
    }

    /**
     * @returns {Value}
     */
    calcBalance() {
        let sum = new Value();

        for (const utxo of this.#utxos) {
            sum = sum.add(utxo.value);
        }

        return sum;
    }

    /**
     * @returns {Address}
     */
    getBaseAddress() {
        return this.#addresses[0];
    }

    /**
     * @returns {Address}
     */
    getChangeAddress() {
        return this.#addresses[this.#changeAddressIndex];
    }

    /**
     * Returns the first UTxO, so the caller can check precisely which network the user is connected to (eg. preprod or preprod)
     * @returns {?UTxO}
     */
    getRefUtxo() {
        if(this.#utxos.length == 0) {
            return null;
        } else {
            return this.#utxos[0]
        }
    }

    /**
     * First picks the UTxO necessary to cover the assets.
     * After that UTxOs to complete the necessary lovelace amount are picked.
     * Uses a simple strategy that picks the smallest UTxOs first
     * Throws error if there aren't enough UTxOs
     * @param {Value} amount
     * @returns {[UTxO[], UTxO[]]} - first: picked, second: not picked that can be used as a backup
     */
    pickUtxos(amount: any) {
        let sum = new Value();

        /** @type {UTxO[]} */
        let notYetPicked = this.#utxos.slice();

        /** @type {UTxO[]} */
        const picked : any[] = [];

        const mphs = amount.assets.mintingPolicies;

        /**
         * Picks smallest utxos until 'needed' is reached
         * @param {bigint} neededQuantity
         * @param {(utxo: UTxO) => bigint} getQuantity
         */
        function picker(neededQuantity: any, getQuantity: any) {
            // first sort notYetPicked in ascending order
            notYetPicked.sort((a:any, b:any) => {
                return Number(getQuantity(a) - getQuantity(b));
            });


            let count = BigInt(0);
            const remaining = [];

            while (count < neededQuantity) {
                const utxo = notYetPicked.shift();

                if (utxo === undefined) {
                    throw new Error("not enough utxos to cover amount");
                } else {
                    const qty = getQuantity(utxo);

                    if (qty > BigInt(0)) {
                        count += qty;
                        picked.push(utxo);
                        sum = sum.add(utxo.value);
                    } else {
                        remaining.push(utxo)
                    }
                }
            }

            notYetPicked = remaining;
        }

        for (const mph of mphs) {
            const tokenNames = amount.assets.getTokenNames(mph);

            for (const tokenName of tokenNames) {
                const need = amount.assets.get(mph, tokenName);
                const have = sum.assets.get(mph, tokenName);

                if (have < need) {
                    const diff = need - have;

                    picker(diff, (utxo: any) => utxo.value.assets.get(mph, tokenName));
                }
            }
        }

        // now use the same strategy for lovelace
        const need = amount.lovelace;
        const have = sum.lovelace;

        if (have < need) {
            const diff = need - have;

            picker(diff, (utxo: any) => utxo.value.lovelace);
        }

        return [picked, notYetPicked];
    }


    /**
     * @param {Address} addr
     * @returns {boolean}
     */
    isOwnAddress(addr: any) {
        const pkh = addr.pubKeyHash;

        if (pkh === null) {
            return false;
        } else {
            return this.isOwnPubKeyHash(pkh);
        }
    }

    /**
     * @param {PubKeyHash} pkh
     * @returns {boolean}
     */
    isOwnPubKeyHash(pkh: any) {
        for (const addr of this.#addresses) {
            const aPkh = addr.pubKeyHash;

            if (aPkh !== null && aPkh.eq(pkh)) {
                return true;
            }
        }

        return false;
    }
}

export class Wallet {
    #initHandle;
    #fullHandle;

    /**
     * @param {{}} initHandle 
     * @param {{}} fullHandle 
     */
    constructor(initHandle: any, fullHandle: any) {
        this.#initHandle = initHandle;
        this.#fullHandle = fullHandle;
    }

    /**
     * @type {string}
     */
    get name() {
        return this.#initHandle.name;
    }

    /**
     * @returns {Promise<number>}
     */
    async getNetworkId() {
        return await this.#fullHandle.getNetworkId();
    }

    /**
     * @returns {Promise<UTxO[]>}
     */
    async getUtxos() {
        // I was honestly expecting this to be some convenient json, but it is in fact CBOR
        const rawUtxos = await this.#fullHandle.getUtxos();

        return rawUtxos.map((rawUtxo: any) => UTxO.fromCbor(hexToBytes(rawUtxo)));
    }

    /**
     * @returns {Promise<[Address[], number]>}
     */
    async getAddresses() {
        let addresses = await this.#fullHandle.getUsedAddresses();

        const changeAddressIndex = addresses.length;

        addresses = addresses.concat(await this.#fullHandle.getUnusedAddresses());

        return [
            addresses.map((a: any) => new Address(hexToBytes(a))),
            changeAddressIndex,
        ];
    }

    /**
     * @returns {Promise<WalletState>}
     */
    async getState() {
        const [addresses, changeAddressIndex] = await this.getAddresses();

        const utxos = await this.getUtxos();

        return new WalletState(addresses, changeAddressIndex, utxos);
    }

    /**
     * @param {Tx} tx
     * @returns {Promise<Signature[]>} signatures
     */
    async signTx(tx: any) {
        const res = await this.#fullHandle.signTx(bytesToHex(tx.toCbor()), true);

        return TxWitnesses.fromCbor(hexToBytes(res)).signatures;
    }

    /**
     * @param {Tx} tx
     * @returns {Promise<string>}
     */
    async submitTx(tx: any) {
        return await this.#fullHandle.submitTx(bytesToHex(tx.toCbor()));
    }
}
