import * as helios from "./helios.js"


const lcValidatorSrc = await Deno.readTextFile("./src/lcValidator.cs");
const programLC = helios.Program.new(lcValidatorSrc);
const simplifyLC = false;
const lcUplcProgramLC = programLC.compile(simplifyLC);
const lcDatum = programLC.evalParam("LC_DATUM");
const lcDatumData = lcDatum.data;
const lcRedeemer = programLC.evalParam("LC_REDEEMER");
const lcRedeemerData = lcRedeemer.data;

//const mphData = helios.UplcData.fromCbor(mph.toCbor());
//programLC.changeParam("TT_MPH", helios.UplcByteArray.new(mphData));
//programLC.changeParam("TT_MPH", mphData);
//programLC.changeParam("TT_MPH", JSON.stringify(["#123"]));
//programLC.changeParam("TT_MPH", JSON.stringify([mph.hex]));
//programLC.evalParam("TT_MPH");

await Deno.writeTextFile("./deploy/redeemer-add-ada.json", lcRedeemerData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-datum-init.json", lcDatumData.toSchemaJson());
await Deno.writeTextFile("./deploy/lc-validator.plutus", lcUplcProgramLC.serialize());

