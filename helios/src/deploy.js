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
await Deno.writeTextFile("./deploy/tt-token-name.json", tn.toSchemaJson());


const lcValidatorSrc = await Deno.readTextFile("./src/lcValidator.cs");
const programLC = helios.Program.new(lcValidatorSrc);
const simplifyLC = false;
const myUplcProgramLC = programLC.compile(simplifyLC);
const myDatum = programLC.evalParam("MY_DATUM")
const myDatumData = myDatum.data
const myRedeemer = programLC.evalParam("MY_REDEEMER")
const myRedeemerData = myRedeemer.data


await Deno.writeTextFile("./deploy/redeemer-spend.json", myRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-datum-init.json", myDatumData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-validator.plutus", myUplcProgramLC.serialize());

