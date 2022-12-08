spending validator

struct Datum {
    adaAmount: Int
    lcAmount: Int

    func get_ratio(self) -> Int {
        self.adaAmount / self.lcAmount
    }
}

enum Redeemer {
    AddAda
    Mint
    Burn { 
        pkhBA: ByteArray 
    }
}

const minAda : Value = Value::lovelace(2000000)


// Define thread token value
const TT_MPH: ByteArray = #e60f99382dc335765cd2207c5611b21880e726d3f875ed0efe312f89
const ttMph: MintingPolicyHash = MintingPolicyHash::new(TT_MPH)
const ttAssetclass: AssetClass = AssetClass::new(
        ttMph, 
        "Thread Token Littercoin".encode_utf8()
    )
const ttVal : Value = Value::new(ttAssetclass, 1)


// Define the mph of the littercoin minting policy
const LC_MPH: ByteArray = #a9a5bb0d4f0e56dcdabdff4e8d9d990bab6bfea60ccf3058a42bffe2
const lcMph: MintingPolicyHash = MintingPolicyHash::new(LC_MPH)
const lcAssetClass: AssetClass = AssetClass::new(
        lcMph, 
        "Littercoin".encode_utf8()
    )

// Define the pkh of the owner
const OWNER_PKH: ByteArray = #b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682 
const ownerPkh: PubKeyHash = PubKeyHash::new(OWNER_PKH)


// Define the merchant token
const MERCHANT_MPH: ByteArray = #e57b84e97afe75117f906e57e66ca0718e25c9db3c4076f2bf78555b
const merchMph: MintingPolicyHash = MintingPolicyHash::new(MERCHANT_MPH)
const merchAssetclass: AssetClass = AssetClass::new(
        merchMph, 
        "Merchant Token Littercoin".encode_utf8()
    )
const merchVal: Value = Value::new(merchAssetclass, 1)


func main(datum: Datum, redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    
    // Get hash of this validator
    vHash : ValidatorHash = ctx.get_current_validator_hash();
    tx : Tx = ctx.tx;
    txOutput : []TxOutput = tx.outputs_locked_by(vHash);

    redeemer.switch {
        AddAda => {
            txOutput.get(0).datum.switch {
                dat: Inline => { 
                    outDat: Datum = Datum::from_data(dat.data);
                    addAdaDatumAmt: Int = outDat.adaAmount - datum.adaAmount;
                    adaVal: Value = Value::lovelace(outDat.adaAmount);

                    // Verify that the total Ada amount from the datum and
                    // the thread token is the same as the output value
                    // locked at the validator address                   
                    print("LCV1" + (tx.value_locked_by(vHash) == (ttVal + adaVal)).show());
                        tx.value_locked_by(vHash) == (ttVal + adaVal) &&
                    (print("LCV2" + (addAdaDatumAmt > 2000000).show()); 
                        addAdaDatumAmt > 2000000)
                },
                else => print("LCV3: invalid datum"); false
            }

        },
        Mint => {
            txOutput.get(0).datum.switch {
                dat: Inline => { 
                    outDat: Datum = Datum::from_data(dat.data);
                    adaDatumAmt: Int = outDat.adaAmount - datum.adaAmount;
                    adaVal: Value = Value::lovelace(outDat.adaAmount);
                    lcDatumAmt: Int = outDat.lcAmount - datum.lcAmount;
                    lcMintVal: Value = Value::new(lcAssetClass, lcDatumAmt);
        
                    // Verify that the total Ada amount from the datum and
                    // the thread token is the same as the output value
                    // locked at the validator address.  Also check the the owner
                    // token is present and returned to back to the owner.                   
                    print("LCV4: " + (adaDatumAmt == 0).show()); 
                        adaDatumAmt == 0 && 
                    (print("LCV5: " + (tx.value_locked_by(vHash) == (ttVal + adaVal)).show()); 
                        tx.value_locked_by(vHash) == (ttVal + adaVal)) &&
                    (print("LCV6: " + (tx.minted.contains(lcMintVal)).show()); 
                        tx.minted.contains(lcMintVal)) && 
                    (print("LCV7: " + (tx.is_signed_by(ownerPkh)).show()); 
                        tx.is_signed_by(ownerPkh)) && 
                    (print("LCV8: " + (0 < lcDatumAmt && lcDatumAmt < outDat.adaAmount).show()); 
                        0 < lcDatumAmt && lcDatumAmt < outDat.adaAmount) 
                },
                else => print("LCV9: invalid datum"); false
            }
        },
        red: Burn => {    
           txOutput.get(0).datum.switch {
                dat: Inline => { 
                    outDat: Datum = Datum::from_data(dat.data);
                    adaDatumAmt: Int = datum.adaAmount - outDat.adaAmount;
                    adaVal: Value = Value::lovelace(outDat.adaAmount);
                    lcDatumAmt: Int = datum.lcAmount - outDat.lcAmount;
                    lcBurnVal: Value = Value::new(lcAssetClass, lcDatumAmt) * (-1);
                    ratio: Int = datum.get_ratio();
                    adaWithdraw : Int = lcDatumAmt * ratio;
                    adaWithdrawVal: Value = Value::lovelace(adaWithdraw);
                    merchPkh: PubKeyHash = PubKeyHash::new(red.pkhBA);
                    merchOutTxs : []TxOutput = tx.outputs_sent_to(merchPkh);
                    
                    // Verify that the amount of littercoin burned is the actual amount
                    // reduced by in the datum and also check that the Ada withdraw
                    // is equal to the amount of Ada remanining in the datum output.
                    // Also confirm that thread token is sent to back to the validator
                    // with correct Ada amount                   
                    (print("LCV10: " + (adaDatumAmt == adaWithdraw).show()); 
                        adaDatumAmt == adaWithdraw) &&
                    (print("LCV11: " + (tx.value_locked_by(vHash) == (ttVal + adaVal)).show()); 
                        tx.value_locked_by(vHash) == (ttVal + adaVal)) &&
                    (print("LCV12: " + (tx.minted.contains(lcBurnVal)).show()); 
                        tx.minted.contains(lcBurnVal)) &&
                    (print("LCV13: " + (merchOutTxs.get(2).value.contains(minAda + merchVal)).show());
                        merchOutTxs.get(2).value.contains(minAda + merchVal)) &&
                    (print("LCV14: " + (merchOutTxs.get(1).value.contains(adaWithdrawVal)).show());
                        merchOutTxs.get(1).value.contains(adaWithdrawVal))


                    //(print("LCV13: " + (tx.value_sent_to(merchPkh).contains(minAda + merchVal)).show()); 
                    //    tx.value_sent_to(merchPkh).contains(minAda + merchVal))
                    //(print("LCV14: " + (tx.value_sent_to(merchPkh).contains(adaWithdrawVal)).show()); 
                    //    tx.value_sent_to(merchPkh).contains(adaWithdrawVal))
                },
                else => print("LCV15: invalid datum"); false
            }
        }
    }
}

const LC_DATUM = Datum {
    lcAmount : 0,
    adaAmount : 0
}

const VAL_ADD_ADA_REDEEMER = Redeemer::AddAda
const VAL_MINT_REDEEMER = Redeemer::Mint
const VAL_BURN_REDEEMER = Redeemer::Burn{#1a2b3c}

