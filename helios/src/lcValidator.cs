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
    Burn
}

// define thread token value
//const TT_MPH : ByteArray = #1a2b3c
//const TT_MPH : String = "8c20cf00f7f840384a8130718c6d7d0057fc24713e5b09df528f66c0"
//const ttMphBA : ByteArray = TT_MPH.serialize()

const ttMph: MintingPolicyHash = MintingPolicyHash::new(#f6d42cc159ac849b444b761e5e7a673a0b24ba813c358a552521e007)
const ttAssetclass: AssetClass = AssetClass::new(
        ttMph, 
        "thread-token".encode_utf8()
    )
const ttVal : Value = Value::new(ttAssetclass, 1)



func main(datum: Datum, redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    redeemer.switch {
        AddAda => {
            // Get hash of this validator
            vHash : ValidatorHash = ctx.get_current_validator_hash();
            tx : Tx = ctx.tx;
            txOutput : []TxOutput = tx.outputs_locked_by(vHash);
            txOutput.get(0).datum.switch {
                d: Inline => { 
                    outDat : Datum = Datum::from_data(d.data);
                    addAdaDatumAmt : Int = outDat.adaAmount - datum.adaAmount;
                    adaVal : Value = Value::lovelace(addAdaDatumAmt);
                    outVal : Value = adaVal + ttVal; 

                    // Verify that the derived value from the datum and
                    // the thread token is the same as the output value
                    // locked at the this validator address                   
                    print("AddAda: Inline");
                    tx.value_locked_by(vHash) == outVal
                },
                else => print("AddAda: else"); false
            }

        },
        Mint => {
            print("Mint: false"); false
        },
        Burn => {
            print("Burn: false"); false
        }

    }
    
}

const LC_DATUM = Datum {
    lcAmount : 0,
    adaAmount : 0
}

const LC_REDEEMER = Redeemer::AddAda
