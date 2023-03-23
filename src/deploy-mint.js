import * as helios from "./helios.js"

const optimize = true;

const mintLCSrc = await Deno.readTextFile("./src/lcMint.hl");
const programMintLC = helios.Program.new(mintLCSrc);
const uplcProgramMintLC = programMintLC.compile(optimize);
const lcMPH = uplcProgramMintLC.mintingPolicyHash;
const lcTokenName = helios.ByteArrayData.fromString("Littercoin");

console.log("littercoin mph: ", lcMPH.hex);
console.log("littercoin token name: ", lcTokenName.toSchemaJson());

const mintLCRedeemer = programMintLC.evalParam("MINT_REDEEMER");
const mintLCRedeemerData = mintLCRedeemer.data;
const burnLCRedeemer = programMintLC.evalParam("BURN_REDEEMER");
const burnLCRedeemerData = burnLCRedeemer.data;

await Deno.writeTextFile("./deploy/lc-mint-redeemer-mint.json", mintLCRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-mint-redeemer-burn.json", burnLCRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-token-name.json", lcTokenName.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-minting-policy.plutus", uplcProgramMintLC.serialize());
await Deno.writeTextFile("./deploy/lc-minting-policy.hash", lcMPH.hex);


const mintReceiptSrc = await Deno.readTextFile("./src/rewardsToken.hl");
const programmintReceipt = helios.Program.new(mintReceiptSrc);
const uplcProgrammintReceipt = programmintReceipt.compile(optimize);
const receiptMPH = uplcProgrammintReceipt.mintingPolicyHash;
const receiptTokenName = helios.ByteArrayData.fromString("Donation Rewards Littercoin");

console.log("Donation rewards mph: ", receiptMPH.hex);
console.log("Donation rewards token name: ", receiptTokenName.toSchemaJson());

const mintReceiptRedeemer = programmintReceipt.evalParam("MINT_REDEEMER");
const mintReceiptRedeemerData = mintReceiptRedeemer.data;

await Deno.writeTextFile("./deploy/rewards-mint-redeemer-mint.json", mintReceiptRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/rewards-mint-token-name.json", receiptTokenName.toSchemaJson());
await Deno.writeTextFile("./deploy/rewards-minting-policy.plutus", uplcProgrammintReceipt.serialize());
await Deno.writeTextFile("./deploy/rewards-minting-policy.hash", receiptMPH.hex);