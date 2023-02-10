import * as helios from "./helios.js"

const lcValidatorSrc = await Deno.readTextFile("./src/lcValidator.hl");
const programLC = helios.Program.new(lcValidatorSrc);
const simplifyLC = false;
const lcUplcProgramLC = programLC.compile(simplifyLC);
const lcDatum = programLC.evalParam("LC_DATUM");
const lcDatumData = lcDatum.data;
const valAddAdaRedeemer = programLC.evalParam("VAL_ADD_ADA_REDEEMER");
const valAddAdaRedeemerData = valAddAdaRedeemer.data;
const valMintRedeemer = programLC.evalParam("VAL_MINT_REDEEMER");
const valMintRedeemerData = valMintRedeemer.data;
const valBurnRedeemer = programLC.evalParam("VAL_BURN_REDEEMER");
const valBurnRedeemerData = valBurnRedeemer.data;
const tn = helios.ByteArrayData.fromString("Merchant Token Littercoin");
console.log("merchant token name: ", tn.toSchemaJson());


const valHash = lcUplcProgramLC.validatorHash;
const valAddr = Address.fromValidatorHash(valHash);
console.log("littercoin validator hash: ", valHash.hex);
console.log("littercoin validator address: ", valAddr.toBech32());

await Deno.writeTextFile("./deploy/redeemer-add-ada.json", valAddAdaRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/redeemer-val-mint.json", valMintRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/redeemer-val-burn.json", valBurnRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-datum-init.json", lcDatumData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-validator.plutus", lcUplcProgramLC.serialize());
await Deno.writeTextFile("./deploy/lc-validator.hash", valHash.hex);
await Deno.writeTextFile("./deploy/lc-validator.addr", valAddr.toBech32());
