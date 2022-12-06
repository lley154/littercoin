import * as helios from "./helios.js"


const mintLCSrc = await Deno.readTextFile("./src/mintLC.cs");
const programMintLC = helios.Program.new(mintLCSrc);
const simplifyMintLC = false;
const uplcProgramMintLC = programMintLC.compile(simplifyMintLC);
const mph = uplcProgramMintLC.mintingPolicyHash;
const tn = helios.ByteArrayData.fromString("Littercoin");

console.log("littercoin mph: ", mph.hex);
console.log("littercoin token name: ", tn.toSchemaJson());

const mintRedeemer = programMintLC.evalParam("MINT_REDEEMER");
const mintRedeemerData = mintRedeemer.data;
const burnRedeemer = programMintLC.evalParam("BURN_REDEEMER");
const burnRedeemerData = burnRedeemer.data;

await Deno.writeTextFile("./deploy/redeemer-mint.json", mintRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/redeemer-burn.json", burnRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-token-name.json", tn.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-minting-policy.plutus", uplcProgramMintLC.serialize());
await Deno.writeTextFile("./deploy/lc-minting-policy.hash", mph.hex);

