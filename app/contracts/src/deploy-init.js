import * as helios from "./helios.js"

const simplify = false;

// Thread Token

const threadTokenSrc = await Deno.readTextFile("./src/threadToken.hl");
const programTT = helios.Program.new(threadTokenSrc);
const myUplcProgramTT = programTT.compile(simplify);
const mphTT = myUplcProgramTT.mintingPolicyHash;
const tnTT = helios.ByteArrayData.fromString("Thread Token Littercoin");

console.log("thread token mph: ", mphTT.hex);
console.log("thread token name: ", tnTT.toString());

const initRedeemerTT = programTT.evalParam("INIT_REDEEMER");
const initRedeemerDataTT = initRedeemerTT.data;

await Deno.writeTextFile("./deploy/tt-redeemer-init.json", initRedeemerDataTT.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-token-name.json", tnTT.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-minting-policy.plutus", myUplcProgramTT.serialize());
await Deno.writeTextFile("./deploy/tt-minting-policy.hash", mphTT.hex);


// Littercoin Token

const tnLC = helios.ByteArrayData.fromString("Littercoin");
console.log("littercoin token name: ", tnLC.toString());

await Deno.writeTextFile("./deploy/lc-token-name.json", tnLC.toSchemaJson());


// Littercoin Metadata

const lcMetatdataSrc = await Deno.readTextFile("./src/lc-token-metadata.json");
await Deno.writeTextFile("./deploy/lc-token-metadata.json", lcMetatdataSrc);


// Merchant Token

const merchTokenSrc = await Deno.readTextFile("./src/merchToken.hl");
const programMT = helios.Program.new(merchTokenSrc);
const myUplcProgramMT = programMT.compile(simplify);
const mphMT = myUplcProgramMT.mintingPolicyHash;
const tnMT = helios.ByteArrayData.fromString("Merchant Token Littercoin");

console.log("merchant token mph: ", mphMT.hex);
console.log("merchant token name: ", tnMT.toString());

await Deno.writeTextFile("./deploy/mt-token-name.json", tnMT.toSchemaJson());
await Deno.writeTextFile("./deploy/mt-minting-policy.plutus", myUplcProgramMT.serialize());
await Deno.writeTextFile("./deploy/mt-minting-policy.hash", mphMT.hex);
