import * as helios from "./helios.js"

const threadTokenSrc = await Deno.readTextFile("./src/threadToken.cs");
const programTT = helios.Program.new(threadTokenSrc);
const simplifyTT = false;
const myUplcProgramTT = programTT.compile(simplifyTT);

await Deno.writeTextFile("./deploy/tt-minting-policy.plutus", myUplcProgramTT.serialize());

const mph = myUplcProgramTT.mintingPolicyHash;

console.log("mph", mph.hex);
await Deno.writeTextFile("./deploy/tt-minting-policy.hash", mph.hex);
//console.log("mph", mph.bytes);

const tn = helios.ByteArrayData.fromString("thread-token");
//console.log("tn", tn.toHex());
//console.log("tn", tn.toCbor());
//console.log("tn", tn.bytes);
console.log("tn", tn.toSchemaJson());

const initRedeemer = programTT.evalParam("INIT_REDEEMER");
const initRedeemerData = initRedeemer.data;
await Deno.writeTextFile("./deploy/redeemer-init.json", initRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-token-name.json", tn.toSchemaJson());

