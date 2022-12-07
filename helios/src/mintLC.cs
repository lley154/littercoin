minting littercoin

enum Redeemer { 
    Mint {
        adaAmount: Int
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


// Define the littercoin validator hash
const VAL_HASH: ByteArray = #60548f3cd4b08189593c426ff4bbac772f2df69e0880c1bf9d374bf5
const vHash : ValidatorHash = ValidatorHash::new(VAL_HASH) 


func main(redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    
    tx: Tx = ctx.tx;

    redeemer.switch {
        m: Mint => {
                // Check that thread token is part of this transaction
                adaVal : Value = ttVal + Value::lovelace(m.adaAmount);
                print("mintLC: Mint");
                tx.value_locked_by(vHash) == adaVal
        },
        Burn => {
            print("mintLC: Burn: false"); false
        }

    }

}

const MINT_REDEEMER = Redeemer::Mint{0}
const BURN_REDEEMER = Redeemer::Burn
