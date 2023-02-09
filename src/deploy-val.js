import * as helios from "./helios.js"

const lcValidatorSrc = await Deno.readTextFile("./src/lcValidator.hl");
const programLC = helios.Program.new(lcValidatorSrc);
const simplify = false;
const lcUplcProgramLC = programLC.compile(simplify);
const lcDatum = programLC.evalParam("LC_DATUM");
const lcDatumData = lcDatum.data;
const valAddAdaRedeemer = programLC.evalParam("VAL_ADD_ADA_REDEEMER");
const valAddAdaRedeemerData = valAddAdaRedeemer.data;
const valMintRedeemer = programLC.evalParam("VAL_MINT_REDEEMER");
const valMintRedeemerData = valMintRedeemer.data;
const valBurnRedeemer = programLC.evalParam("VAL_BURN_REDEEMER");
const valBurnRedeemerData = valBurnRedeemer.data;

const vHash = lcUplcProgramLC.validatorHash;
console.log("littercoin validator hash: ", vHash.hex);
console.log("littercoin validator address: ", helios.Address.fromValidatorHash(vHash).toBech32());

await Deno.writeTextFile("./deploy/lc-redeemer-add-ada.json", valAddAdaRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-redeemer-mint.json", valMintRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-redeemer-burn.json", valBurnRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-datum-init.json", lcDatumData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-validator.plutus", lcUplcProgramLC.serialize());
await Deno.writeTextFile("./deploy/lc-validator.hash", vHash.hex);
