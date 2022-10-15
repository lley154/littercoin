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
    echo "transfer-funds-tx.sh:  Invalid script arguments"
    echo "Usage: transfer-funds-tx.sh [devnet|testnet]"
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

echo "starting littercoin thread token mint"
echo "Script: $minting_script"

###############################################
# Devnet prep - set up utxos for admin and user
###############################################


# Step 1: Get UTXOs from gensis utxos
# There needs to be at least 2 utxos that can be consumed; one for minting of the token
# and one uxto for collateral
gen_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$GEN_VKEY")
$CARDANO_CLI query utxo --address "$gen_utxo_addr" --cardano-mode $network --out-file $WORK/gen-utxo.json
cat $WORK/gen-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/gen-utxo-valid.json
readarray gen_utxo_valid_array < $WORK/gen-utxo-valid.json
gen_utxo_in=$(echo $gen_utxo_valid_array | tr -d '\n')
admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")


# Step 2: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$gen_utxo_addr" \
  --tx-in "$gen_utxo_in" \
  --tx-out "$admin_utxo_addr+100000000" \
  --tx-out "$admin_utxo_addr+10000000" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/transfer-tx-alonzo.body
  
echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/transfer-tx-alonzo.body \
  $network \
  --signing-key-file "${GEN_SKEY}" \
  --out-file $WORK/transfer-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/transfer-tx-alonzo.tx $network


