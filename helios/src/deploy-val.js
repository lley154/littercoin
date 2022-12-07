import * as helios from "./helios.js"

const lcValidatorSrc = await Deno.readTextFile("./src/lcValidator.cs");
const programLC = helios.Program.new(lcValidatorSrc);
const simplifyLC = false;
const lcUplcProgramLC = programLC.compile(simplifyLC);
const lcDatum = programLC.evalParam("LC_DATUM");
const lcDatumData = lcDatum.data;
const valAddAdaRedeemer = programLC.evalParam("VAL_ADD_ADA_REDEEMER");
const valAddAdaRedeemerData = valAddAdaRedeemer.data;
const valMintRedeemer = programLC.evalParam("VAL_MINT_REDEEMER");
const valMintRedeemerData = valMintRedeemer.data;

//const mphData = helios.UplcData.fromCbor(mph.toCbor());
//programLC.changeParam("TT_MPH", helios.UplcByteArray.new(mphData));
//programLC.changeParam("TT_MPH", mphData);
//programLC.changeParam("TT_MPH", JSON.stringify(["#123"]));
//programLC.changeParam("TT_MPH", JSON.stringify([mph.hex]));

const vHash = lcUplcProgramLC.validatorHash;
console.log("littercoin validator hash: ", vHash.hex);

await Deno.writeTextFile("./deploy/redeemer-add-ada.json", valAddAdaRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/redeemer-val-mint.json", valMintRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-datum-init.json", lcDatumData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-validator.plutus", lcUplcProgramLC.serialize());
await Deno.writeTextFile("./deploy/lc-validator.hash", vHash.hex);
