#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "mint-littercoin-tx.sh:  Invalid script arguments"
    echo "Usage: mint-littercoin-tx.sh [devnet|testnet|mainnet]"
    exit 1
fi
ENV=$1

# Pull in global export variables
MY_DIR=$(dirname $(readlink -f $0))
source $MY_DIR/$ENV/global-export-variables.sh

if [ "$ENV" == "mainnet" ];
then
    network="--mainnet"
else
    network="--testnet-magic $TESTNET_MAGIC"
fi

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"

ls -al "$CARDANO_NODE_SOCKET_PATH"

mkdir -p $WORK
mkdir -p $WORK-backup
rm -f $WORK/*
rm -f $WORK-backup/*


# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json

# load in local variable values
nft_mint_script="$BASE/scripts/cardano-cli/$ENV/data/nft-minting-policy.plutus"
nft_mint_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/nft-minting-policy.hash | jq -r '.bytes')
nft_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/nft-token-name.json | jq -r '.bytes')
redeemer_nft_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-mint-nft.json"

admin_pkh=$(cat $ADMIN_PKH)

# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for minting of the token
# and one uxto for collateral
admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json
cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')


# Step 2: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --tx-in "$admin_utxo_in" \
  --mint "1 $nft_mint_mph.$nft_token_name" \
  --mint-script-file "$nft_mint_script" \
  --mint-redeemer-file "$redeemer_nft_file_path" \
  --tx-out "$admin_utxo_addr+$MIN_ADA_OUTPUT_TX + 1 $nft_mint_mph.$nft_token_name" \
  --required-signer-hash "$admin_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/mint-nft-tx-alonzo.body

#  --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/add-ada.costs"
  

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/mint-nft-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/mint-nft-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/mint-nft-tx-alonzo.tx $network




