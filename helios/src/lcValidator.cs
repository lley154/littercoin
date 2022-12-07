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
    Mint {
        pkh: PubKeyHash 
    }
    Burn
}

const minAda : Value = Value::lovelace(2000000)


// Define thread token value
const TT_MPH: ByteArray = #1260cf2650e19e8353e21383fa466b83b8a7cf10e3a56aeb4904eb60
const ttMph: MintingPolicyHash = MintingPolicyHash::new(TT_MPH)
const ttAssetclass: AssetClass = AssetClass::new(
        ttMph, 
        "thread-token".encode_utf8()
    )
const ttVal : Value = Value::new(ttAssetclass, 1)

// Define the owner token
const OWNER_MPH: ByteArray = #e57b84e97afe75117f906e57e66ca0718e25c9db3c4076f2bf78555b
const ownerMph: MintingPolicyHash = MintingPolicyHash::new(OWNER_MPH)
const ownerAssetclass: AssetClass = AssetClass::new(
        ownerMph, 
        "Owner Token Littercoin".encode_utf8()
    )
const ownerVal: Value = Value::new(ownerAssetclass, 1)


func main(datum: Datum, redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    
    // Get hash of this validator
    vHash : ValidatorHash = ctx.get_current_validator_hash();
    tx : Tx = ctx.tx;
    txOutput : []TxOutput = tx.outputs_locked_by(vHash);

    redeemer.switch {
        AddAda => {
            txOutput.get(0).datum.switch {
                d: Inline => { 
                    outDat : Datum = Datum::from_data(d.data);
                    addAdaDatumAmt : Int = outDat.adaAmount - datum.adaAmount;
                    adaVal : Value = Value::lovelace(addAdaDatumAmt);

                    // Verify that the total Ada amount from the datum and
                    // the thread token is the same as the output value
                    // locked at the validator address                   
                    print("lcValidator: AddAda: Inline");
                    tx.value_locked_by(vHash) == (ttVal + adaVal)
                },
                else => print("lcValidator: AddAda: else"); false
            }

        },
        m: Mint => {
            txOutput.get(0).datum.switch {
                d: Inline => { 
                    outDat : Datum = Datum::from_data(d.data);
                    adaDatumAmt : Int = outDat.adaAmount - datum.adaAmount;
                    adaVal : Value = Value::lovelace(outDat.adaAmount);
        
                    // Verify that the total Ada amount from the datum and
                    // the thread token is the same as the output value
                    // locked at the validator address.  Also check the the owner
                    // token is present and returned to back to the owner.                   
                    print("lcValidator: Mint: Inline");
                    adaDatumAmt == 0 &&  
                    (print("lcValidator: Mint: tx.value_locked_by: " + (tx.value_locked_by(vHash) == (ttVal + adaVal)).show()); tx.value_locked_by(vHash) == (ttVal + adaVal)) &&
                    (print("lcValidator: Mint: tx.value_sent_to: " + m.pkh.show()); tx.value_sent_to(m.pkh) == (minAda + ownerVal))
                },
                else => print("lcValidator: Mint: else"); false
            }
        },
        Burn => {
            print("lcValidator: Burn: false"); false
        }

    }
    
}

const LC_DATUM = Datum {
    lcAmount : 0,
    adaAmount : 0
}

// Define the pkh of the owner
const OWNER_PKH_BA: ByteArray = #b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682 
const OWNER_PKH: PubKeyHash = PubKeyHash::new(OWNER_PKH_BA)

const VAL_ADD_ADA_REDEEMER = Redeemer::AddAda
const VAL_MINT_REDEEMER = Redeemer::Mint{ OWNER_PKH }
