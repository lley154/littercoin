minting threadToken

enum Redeemer { 
    Init 
}

const txIdBA: ByteArray = #7324ba2042de81bd05614764b4b197cb6b9f3e058178da313fd3279a0919f605
const txId: TxId = TxId::new(txIdBA)
const OUTPUT_ID: TxOutputId = TxOutputId::new(txId, 0)

func main(ctx: ScriptContext) -> Bool {
    tx: Tx = ctx.tx;
    mph: MintingPolicyHash = ctx.get_current_minting_policy_hash();

    tt_assetclass: AssetClass = AssetClass::new(
        mph, 
        "thread-token".encode_utf8()
    );

    value_minted: Value = tx.minted;

    txIdBA2: ByteArray = txId.serialize();
    txIdHex: String = txIdBA2.show();

    print(txIdHex);
        (value_minted == Value::new(tt_assetclass, 1) &&
            (print("minted");
                (tx.inputs
                    .any((input: TxInput) -> Bool {print ("input"); input.output_id == OUTPUT_ID})
                )
            ) 

        )

}

const INIT_REDEEMER = Redeemer::Init
