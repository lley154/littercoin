import * as helios from "./helios.js"

// Set compiler optimizer flag
const optimize = false;

// Thread token
const threadTokenSrc = await Deno.readTextFile("./src/threadToken.hl");
const programTT = helios.Program.new(threadTokenSrc);

const myUplcProgramTT = programTT.compile(optimize);
const mph = myUplcProgramTT.mintingPolicyHash;
const tn = helios.ByteArrayData.fromString("Thread Token Littercoin");

console.log("thread token mph: ", mph.hex);
console.log("thread token name: ", tn.toSchemaJson());

const initRedeemer = programTT.evalParam("INIT_REDEEMER");
const initRedeemerData = initRedeemer.data;

await Deno.writeTextFile("./deploy/redeemer-init.json", initRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-token-name.json", tn.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-minting-policy.plutus", myUplcProgramTT.serialize());
await Deno.writeTextFile("./deploy/tt-minting-policy.hash", mph.hex);

const lcMetatdataSrc = await Deno.readTextFile("./src/lc-token-metadata.json");
await Deno.writeTextFile("./deploy/lc-token-metadata.json", lcMetatdataSrc);


// Merchant Token
const merchTokenSrc = await Deno.readTextFile("./src/merchToken.hl");
const programMT = helios.Program.new(merchTokenSrc);
const myUplcProgramMT = programMT.compile(optimize);
const mphMT = myUplcProgramMT.mintingPolicyHash;
const tnMT = helios.ByteArrayData.fromString("Merchant Token Littercoin");

console.log("merchant token mph: ", mphMT.hex);
console.log("merchant token name: ", tnMT.toString());

await Deno.writeTextFile("./deploy/mt-token-name.json", tnMT.toSchemaJson());
await Deno.writeTextFile("./deploy/mt-minting-policy.plutus", myUplcProgramMT.serialize());
await Deno.writeTextFile("./deploy/mt-minting-policy.hash", mphMT.hex);