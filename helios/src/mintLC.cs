minting littercoin

enum Redeemer { 
    Mint {
        vHashBA: ByteArray
    }
    Burn
 }

const minAda : Value = Value::lovelace(2000000)


// Define thread token value
const TT_MPH: ByteArray = #71d164974ddf71f49a95d022d2a94329d3afa1a8a5ab1b1f7b96c05c
const ttMph: MintingPolicyHash = MintingPolicyHash::new(TT_MPH)
const ttAssetclass: AssetClass = AssetClass::new(
        ttMph, 
        "thread-token".encode_utf8()
    )
const ttVal : Value = Value::new(ttAssetclass, 1)


// Define the littercoin validator hash
//const VAL_HASH: ByteArray = #5431bf940cfb0d6346457d42ae920dcc69011463ec94c0fa9bff74e6
//const vHash : ValidatorHash = ValidatorHash::new(VAL_HASH) 


func main(redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    
    tx: Tx = ctx.tx;

    redeemer.switch {
        m: Mint => {
                // Check that thread token is part of this transaction
                //adaVal : Value = ttVal + Value::lovelace(m.adaAmount);
                vHash: ValidatorHash = ValidatorHash::new(m.vHashBA); 
                
                //tx.value_locked_by(vHash).contains(tx.minted)
                print("mintLC: Mint: value_locked_by: " + vHash.show()); tx.value_locked_by(vHash).contains(ttVal)
        },
        Burn => {
            print("mintLC: Burn: false"); false
        }

    }

}

const MINT_REDEEMER = Redeemer::Mint{#1a2b3c}
const BURN_REDEEMER = Redeemer::Burn
