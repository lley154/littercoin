minting threadToken

enum Redeemer { 
    Init 
}

const TX_ID: ByteArray = #fbfe5a81cdc84d032bfa3c4e219c0a69a2b12d6e1246341121473c0e94efa0c9
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
