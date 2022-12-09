minting threadToken

enum Redeemer { 
    Init 
}

const TX_ID: ByteArray = #57cd9395c30985c4f5850b97fd42ab72750fe37700256549f4476626ef9338b2
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
                                    print("TT1: " + (input.output_id == outputId).show());
                                        input.output_id == outputId
                                    }
    )

}

const INIT_REDEEMER = Redeemer::Init
