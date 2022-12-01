#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $3 ]; 
then
    echo "add-ada-tx.sh:  Invalid script arguments"
    echo "Usage: add-ada-tx.sh [devnet|preview|preprod|mainnet] txHash txIndx"
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



# Specify the utxo at the smart contract address we want to use
action_utxo_in=$2
action_utxo_in_txid=$action_utxo_in#$3


# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json

# load in local variable values
action_validator_script="$BASE/scripts/cardano-cli/$ENV/data/action-validator.plutus"
action_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$action_validator_script" $network)
lc_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lc-validator.plutus"
lc_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lc_validator_script" $network)
redeemer_add_ada_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-add-ada.json"
redeemer_spend_action_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-spend-action.json"
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-minting-policy.hash | jq -r '.bytes')
thread_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-name.json | jq -r '.bytes')
admin_pkh=$(cat $ADMIN_PKH)


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


# Step 2: Get the action utxos
$CARDANO_CLI query utxo --address $action_validator_script_addr $network --out-file $WORK/action-utxo.json

add_ada_datum_in=$(jq -r 'to_entries[] 
| select(.key == "'$action_utxo_in_txid'") 
| .value.inlineDatum' $WORK/action-utxo.json)

echo -n "$add_ada_datum_in" > $WORK/add-ada-datum-in.json


# Get the sequence number and Ada amount from the action datum
action_sequence_num=$(jq -r '.fields[0].int' $WORK/add-ada-datum-in.json)
add_ada_amount=$(jq -r '.fields[1].int' $WORK/add-ada-datum-in.json)
dest_payment_key_encoded=$(jq -r '.fields[2].bytes' $WORK/add-ada-datum-in.json)
dest_payment_key=$(echo -n "$dest_payment_key_encoded=" | xxd -r -p)
dest_stake_key_encoded=$(jq -r '.fields[3].bytes' $WORK/add-ada-datum-in.json)
dest_stake_key=$(echo -n "$dest_stake_key_encoded=" | xxd -r -p)

dest_addr=$($BECH32 $ADDR_PREFIX <<< $BECH32_NETWORK$dest_payment_key$dest_stake_key)

# Get the littercoin smart contract utxos
$CARDANO_CLI query utxo --address $lc_validator_script_addr $network --out-file $WORK/lc-validator-utxo.json


# Pull the utxo with the thread token in it
lc_validator_utxo_tx_in=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$thread_token_name'") 
| .key' $WORK/lc-validator-utxo.json)


# Pull the datum with the thread token attached to the utxo
lc_validator_datum_in=$(jq -r 'to_entries[] 
| select(.value.value."'$thread_token_mph'"."'$thread_token_name'") 
| .value.inlineDatum' $WORK/lc-validator-utxo.json)

# Save the inline datum to disk
echo -n "$lc_validator_datum_in" > $WORK/lc-datum-in.json


# get the current total Ada and Littercoin amount in the smart contract
total_ada=$(jq -r '.fields[0].int' $WORK/lc-datum-in.json)
total_lc=$(jq -r '.fields[1].int' $WORK/lc-datum-in.json)
new_total_ada=$(($total_ada + $add_ada_amount))


# Update the littercoin datum accordingly
cat $WORK/lc-datum-in.json | \
jq -c '
  .fields[0].int   |= '$new_total_ada'' > $WORK/lc-datum-out.json


# Upate the redeemer for the lc validator and include the sequence number
cat $redeemer_add_ada_file_path | \
jq -c '
  .fields[0].int          |= '$action_sequence_num'' > $WORK/redeemer-add-ada.json


# Upate the redeemer for the action validator and include the total ada locked at the lc smart contract
cat $redeemer_spend_action_file_path | \
jq -c '
  .fields[0].int          |= '$new_total_ada'' > $WORK/redeemer-action.json




# Step 3: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$admin_utxo_collateral_in" \
  --tx-in "$admin_utxo_in" \
  --tx-in "$lc_validator_utxo_tx_in" \
  --spending-tx-in-reference "$LC_VAL_REF_SCRIPT" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$WORK/redeemer-add-ada.json" \
  --tx-out "$lc_validator_script_addr+$new_total_ada + 1 $thread_token_mph.$thread_token_name" \
  --tx-out-inline-datum-file "$WORK/lc-datum-out.json"  \
  --tx-in "$action_utxo_in_txid" \
  --spending-tx-in-reference "$ACTION_VAL_REF_SCRIPT" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$WORK/redeemer-action.json" \
  --tx-out "$dest_addr+$MIN_ADA_OUTPUT_TX + 1 $DONOR_TOKEN_MPH.$DONOR_TOKEN_NAME" \
  --tx-out-inline-datum-file "$WORK/lc-datum-out.json"  \
  --required-signer-hash "$admin_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/add-ada-tx-alonzo.body


# --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/add-ada-tx-alonzo.costs"


echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/add-ada-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/add-ada-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/add-ada-tx-alonzo.tx $network



