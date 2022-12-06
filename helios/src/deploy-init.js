import * as helios from "./helios.js"

const threadTokenSrc = await Deno.readTextFile("./src/threadToken.cs");
const programTT = helios.Program.new(threadTokenSrc);
const simplifyTT = false;
const myUplcProgramTT = programTT.compile(simplifyTT);
const mph = myUplcProgramTT.mintingPolicyHash;
const tn = helios.ByteArrayData.fromString("thread-token");

console.log("thread token mph: ", mph.hex);
console.log("thread token name: ", tn.toSchemaJson());

const initRedeemer = programTT.evalParam("INIT_REDEEMER");
const initRedeemerData = initRedeemer.data;

await Deno.writeTextFile("./deploy/redeemer-init.json", initRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-token-name.json", tn.toSchemaJson());
await Deno.writeTextFile("./deploy/tt-minting-policy.plutus", myUplcProgramTT.serialize());
await Deno.writeTextFile("./deploy/tt-minting-policy.hash", mph.hex);
