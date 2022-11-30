#!/usr/bin/env bash

##############################################################
# You must do these steps first before running this script
##############################################################
#
# Step 1.   Confirm you have 2 UTXO at admin address (5 Ada for Collateral, and anything greater than 5 Ada)
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
thread_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-name.json | jq -r '.bytes')
lc_mint_script="$BASE/scripts/cardano-cli/$ENV/data/lc-minting-policy.plutus"
lc_mint_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lc-minting-policy.hash | jq -r '.bytes')
lc_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lc-token-name.json | jq -r '.bytes')
lc_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lc-validator.plutus"
lc_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lc_validator_script" $network)
redeemer_tt_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-thread-token-mint.json"
redeemer_lc_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-mint-lc.json"

echo "starting littercoin init-tx.sh"

echo $lc_validator_script_addr > $BASE/scripts/cardano-cli/$ENV/data/lc-validator.addr


################################################################
# Mint the threadtoken and attach it to the littercoin contract
################################################################

# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for spending of the token
# and one uxto for collateral

admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json

cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')

cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace == '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-collateral-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-collateral-valid.json
admin_utxo_collateral_in=$(echo $admin_utxo_valid_array | tr -d '\n')

reserve_lc=$(jq -r '.fields[2].int' "$BASE/scripts/cardano-cli/$ENV/data/lc-datum-init.json")


# Step 2: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$admin_utxo_collateral_in" \
  --tx-in "$admin_utxo_in" \
  --mint-script-file "$thread_token_script" \
  --mint-redeemer-file "$redeemer_tt_file_path" \
  --mint-script-file "$lc_mint_script" \
  --mint-redeemer-file "$redeemer_lc_file_path" \
  --mint "1 $thread_token_mph.$thread_token_name + $reserve_lc $lc_mint_mph.$lc_token_name" \
  --tx-out "$lc_validator_script_addr+$MIN_ADA_OUTPUT_TX + 1 $thread_token_mph.$thread_token_name + $reserve_lc $lc_mint_mph.$lc_token_name" \
  --tx-out-inline-datum-file "$BASE/scripts/cardano-cli/$ENV/data/lc-datum-init.json" \
  --tx-out "$lc_validator_script_addr+$MIN_ADA_OUTPUT_TX_REF" \
  --tx-out-reference-script-file "$lc_validator_script" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/init-tx-alonzo.body


echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/init-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/init-tx-alonzo.tx

echo "tx has been signed"

#echo "Submit the tx with plutus script and wait 5 seconds..."
#$CARDANO_CLI transaction submit --tx-file $WORK/init-tx-alonzo.tx $network




