minting utxo_nft

const txId : TxId = TxId::new(#1e7e37baaa6c0d66e3f90634fc3ba71080156a894a22271af16b7be634e39ceb)
const OUTPUT_ID: TxOutputId = TxOutputId::new(txId, 0)

func main(ctx: ScriptContext) -> Bool {
    tx: Tx = ctx.tx;
    mph : MintingPolicyHash = ctx.get_current_minting_policy_hash();

    nft_assetclass: AssetClass = AssetClass::new(
        mph, 
        "thread-token".encode_utf8()
    );

    value_minted: Value = tx.minted;

    txIdBA: ByteArray = txId.serialize();
    txIdHex: String = txIdBA.show();

    print(txIdHex);
        (value_minted == Value::new(nft_assetclass, 1) &&
            (print("minted");
                (tx.inputs
                    .any((input: TxInput) -> Bool {print ("input"); input.output_id == OUTPUT_ID})
                )
            ) 

        )

}