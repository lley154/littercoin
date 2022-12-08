minting threadToken

enum Redeemer { 
    Init 
}

const TX_ID: ByteArray = #033cf4b3a26a3fd49291b2bb618a6fc96b0ea5a9306904641d72f3e41a8e3189
const txId: TxId = TxId::new(TX_ID)
const outputId: TxOutputId = TxOutputId::new(txId, 0)

func main(ctx: ScriptContext) -> Bool {
    tx: Tx = ctx.tx;
    mph: MintingPolicyHash = ctx.get_current_minting_policy_hash();

    tt_assetclass: AssetClass = AssetClass::new(
        mph, 
        "Thread Token Littercoin".encode_utf8()
    );

    value_minted: Value = tx.minted;
    value_minted == Value::new(tt_assetclass, 1) &&
    tx.inputs.any((input: TxInput) -> Bool {
                                    print("threadToken: input.output_id: " + (input.output_id == outputId).show());
                                        input.output_id == outputId
                                    }
    )

}

const INIT_REDEEMER = Redeemer::Init
