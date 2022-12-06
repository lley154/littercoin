minting littercoin

enum Redeemer { 
    Mint {
        adaAmount: Int
    }
    Burn
 }

const minAda : Value = Value::lovelace(2000000)


// Define thread token value
const ttMphBA: ByteArray = #e500a7ebc9257a5ae64b915a65ccca8312dde842d8b6707cca9e50abc6c177d0
const ttMph: MintingPolicyHash = MintingPolicyHash::new(ttMphBA)
const ttAssetclass: AssetClass = AssetClass::new(
        ttMph, 
        "thread-token".encode_utf8()
    )
const ttVal : Value = Value::new(ttAssetclass, 1)


// Define the littercoin validator hash
const vHashBA: ByteArray = #c38e85d220eb25952f13b4b2b912d8b703c199c96a15037f20bae0e0
const vHash : ValidatorHash = ValidatorHash::new(vHashBA) 


func main(redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    
    tx: Tx = ctx.tx;

    redeemer.switch {
        m: Mint => {
                // Check that thread token is part of this transaction
                adaVal : Value = ttVal + Value::lovelace(m.adaAmount);
                print("Mint");
                tx.value_locked_by(vHash) == adaVal
        },
        Burn => {
            print("Burn: false"); false
        }

    }

}

const MINT_REDEEMER = Redeemer::Mint{0}
const BURN_REDEEMER = Redeemer::Burn
