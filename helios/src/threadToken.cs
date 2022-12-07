minting threadToken

enum Redeemer { 
    Init 
}

const TX_ID: ByteArray = #75e7b3cbaaa90456f11184da9b11146809ae3cf1a8d4879894e1a375e95aa827
const txId: TxId = TxId::new(TX_ID)
const outputId: TxOutputId = TxOutputId::new(txId, 0)

func main(ctx: ScriptContext) -> Bool {
    tx: Tx = ctx.tx;
    mph: MintingPolicyHash = ctx.get_current_minting_policy_hash();

    tt_assetclass: AssetClass = AssetClass::new(
        mph, 
        "thread-token".encode_utf8()
    );

    value_minted: Value = tx.minted;
    value_minted == Value::new(tt_assetclass, 1) &&
    tx.inputs.any((input: TxInput) -> Bool {
                                    input.output_id == outputId
                                    }
    )

}

const INIT_REDEEMER = Redeemer::Init
