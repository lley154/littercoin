import * as helios from "./helios.js"

const src = await Deno.readTextFile("./threadToken.cs");
console.log(src);

const program = helios.Program.new(src);
const simplify = true
const myUplcProgram = program.compile(simplify);

const mph = myUplcProgram.mintingPolicyHash;
const tn = helios.ByteArrayData.fromString("thread-token");
const adminAddress = helios.Address.fromBech32("addr_test1vzu6hnmgvageu2qyypy25yfqwg222tndt5eg3d6j68p8dqspgdxn7");


const minAda = new helios.Value(2000000n);
//const ttAsset = new helios.Assets([mph, 1n])
const qty = 1n;
const ttAsset = new helios.Assets([[mph, [[tn, qty]]]]);
const ttVal = new helios.Value(ttAsset);
const outVal = minAda + ttVal;

const output = new helios.TxOutput(
    adminAddress,
    outVal, 
)


const utxo = new helios.UTxO(
    helios.TxId.fromHex("1e7e37baaa6c0d66e3f90634fc3ba71080156a894a22271af16b7be634e39ceb"), // hash of the tx that created the utxo
    0n, // utxo index as bigint
    new helios.TxOutput(
        adminAddress,
        new helios.Value(2997411403n), // 1 tAda == 1 million lovelace
    )  
)

const utxo_collat = new helios.UTxO(
    helios.TxId.fromHex("705695e0f0bb183929c07c757a147aae543b775c685bd876e0c5794a24ce2344"), // hash of the tx that created the utxo
    0n, // utxo index as bigint
    new helios.TxOutput(
        adminAddress,
        new helios.Value(5000000n), // 1 tAda == 1 million lovelace
    )  
)

const tx = new helios.Tx();

tx.addInput(utxo);
tx.addCollateral(utxo_collat);
tx.mintTokens(
        myUplcProgram.mintingPolicyHash, 
        [[tn, 1n]],
    );
tx.attachScript(myUplcProgram);
tx.addOutput(output);


// in an async context

const networkParams = new helios.NetworkParams(
    await fetch("https://d1t0d7c2nekuk0.cloudfront.net/preprod.json")
        .then(response => response.json())
)


// async because scripts are evaluated asyncronously

await tx.finalize(networkParams, adminAddress)

// in an async context

const response = await walletHandle.signTx(helios.bytesToHex(tx.toCbor()), true)

// extract the deserialized signatures
const signatures = helios.TxWitnesses.fromCbor(helios.hexToBytes(response)).signatures

tx.addSignatures(signatures)

// in async context

// returns the hash of the tx
await walletHandle.submitTx(helios.bytesToHex(tx.toCbor()))



