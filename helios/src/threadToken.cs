minting utxo_nft

enum Redeemer { Init }

const txId : TxId = TxId::new(#f185994952f937981fe590028ad2580442042584396e6119639b65949215c0c8)
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

const INIT_REDEEMER = Redeemer::Init
