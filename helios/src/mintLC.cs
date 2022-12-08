minting littercoin

enum Redeemer { 
    Mint {
        lcValHashBA: ByteArray
    }
    Burn {
        lcValHashBA: ByteArray
    }
 }

const minAda : Value = Value::lovelace(2000000)


// Define thread token value
const TT_MPH: ByteArray = #f4d3da48eb4823914c03ff147660aa35c0eb91b117bcb5d3f8e2d465
const ttMph: MintingPolicyHash = MintingPolicyHash::new(TT_MPH)
const ttAssetclass: AssetClass = AssetClass::new(
        ttMph, 
        "Thread Token Littercoin".encode_utf8()
    )
const ttVal : Value = Value::new(ttAssetclass, 1)


func main(redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    
    tx: Tx = ctx.tx;

    redeemer.switch {
        red: Mint => {
                // Check that thread token is part of this transaction
                vHash: ValidatorHash = ValidatorHash::new(red.lcValHashBA); 
                print("mintLC: Mint: value_locked_by: " + (tx.value_locked_by(vHash).contains(ttVal)).show()); 
                    tx.value_locked_by(vHash).contains(ttVal)
        },
        red: Burn => {
                // Check that thread token is part of this transaction
                vHash: ValidatorHash = ValidatorHash::new(red.lcValHashBA); 
                print("mintLC: Burn: value_locked_by: " + (tx.value_locked_by(vHash).contains(ttVal)).show()); 
                    tx.value_locked_by(vHash).contains(ttVal)
        }
    }
}

const MINT_REDEEMER = Redeemer::Mint{#1a2b3c}
const BURN_REDEEMER = Redeemer::Burn{#1a2b3c}
