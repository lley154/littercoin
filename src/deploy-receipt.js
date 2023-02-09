import * as helios from "./helios.js"

const receiptSrc = await Deno.readTextFile("./src/receiptToken.hl");
const programReceipt = helios.Program.new(receiptSrc);
const optimize = false;
const uplcProgramReceipt = programReceipt.compile(optimize);
const mph = uplcProgramReceipt.mintingPolicyHash;
const tn = helios.ByteArrayData.fromString("Littercoin Donation");

console.log("receipt mph: ", mph.hex);
console.log("receipt token name: ", tn.toSchemaJson());

const mintRedeemer = programReceipt.evalParam("MINT_REDEEMER");
const mintRedeemerData = mintRedeemer.data;

await Deno.writeTextFile("./deploy/receipt-redeemer-mint.json", mintRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/receipt-token-name.json", tn.toSchemaJson());
await Deno.writeTextFile("./deploy/receipt-minting-policy.plutus", uplcProgramReceipt.serialize());
await Deno.writeTextFile("./deploy/receipt-minting-policy.hash", mph.hex);
