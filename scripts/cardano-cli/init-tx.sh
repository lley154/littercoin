#!/usr/bin/env bash

##############################################################
# You must do these steps first before running this script
##############################################################
#
# Step 1.   Find the admin UTXO (and admin pkh) you will use
# Step 2.   Update src/Littercoin/Deploy.hs with that UTXO (and admin pkh) 
# Step 3.   nix-shell, cabal repl, main
# Step 4.   Copy deploy/* scripts/cardano-cli/[devnet|testnet|mainnet]/data
# Step 5.   Update scripts/cardano-cli/[devnet|testnet|mainnet]/global-export-variables.sh
#           with the UTXO to be used for admin collateral
##############################################################


# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "init-tx.sh:  Invalid script arguments"
    echo "Usage: init-tx.sh [devnet|testnet|mainnet]"
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
thread_token_script="$BASE/scripts/cardano-cli/$ENV/data/thread-token-minting-policy.plutus"
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-minting-policy.hash | jq -r '.bytes')
redeemer_thread_token_mint_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-thread-token-mint.json"
thread_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-name.json | jq -r '.bytes')
#token_metadata_file_path="$BASE/scripts/cardano-cli/$ENV/data/token-metadata.json"
lc_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lc-validator.plutus"
lc_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lc_validator_script" $network)

admin_pkh=$(cat $ADMIN_PKH)


echo "starting littercoin thread token mint"
echo "Script: $thread_token_script"

################################################################
# Mint the threadtoken and attach it to the littercoin contract
################################################################

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
  --mint "1 $thread_token_mph.$thread_token_name" \
  --mint-script-file "$thread_token_script" \
  --mint-redeemer-file "$redeemer_thread_token_mint_file_path" \
  --tx-out "$lc_validator_script_addr+$MIN_ADA_OUTPUT_TX + 1 $thread_token_mph.$thread_token_name" \
  --tx-out-inline-datum-file "$BASE/scripts/cardano-cli/$ENV/data/lc-datum-init.json"  \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/tt-token-mint-tx-alonzo.body
  
# --tx-out "$admin_utxo_addr+$MIN_ADA_OUTPUT_TX + 1 $thread_token_mph.$thread_token_name" \
# --required-signer-hash "$admin_pkh" \
# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/token-mint-alonzo.costs"

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/tt-token-mint-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/tt-token-mint-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/tt-token-mint-tx-alonzo.tx $network




