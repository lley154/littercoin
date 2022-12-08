#!/usr/bin/env bash

##################################################################
# You must choose a utxo that contains only Ada and not any NFTs
##################################################################

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "add-ada-tx.sh:  Invalid script arguments"
    echo "Usage: add-ada-tx.sh [devnet|testnet|mainnet]"
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
thread_token_script="$BASE/scripts/$ENV/data/tt-minting-policy.plutus"
thread_token_mph=$(cat $BASE/scripts/$ENV/data/tt-minting-policy.hash)
thread_token_name=$(cat $BASE/scripts/$ENV/data/tt-token-name.json | jq -r '.bytes')
lc_validator_script="$BASE/scripts/$ENV/data/lc-validator.plutus"
lc_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lc_validator_script" $network)
redeemer_file_path="$BASE/scripts/$ENV/data/redeemer-add-ada.json"

ada_amount=10000000

echo "starting littercoin add-ada-tx.sh"

echo $lc_validator_script_addr > $BASE/scripts/$ENV/data/lc-validator.addr


################################################################
# Send Ada to the littercoin contract
################################################################

# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for minting of the token
# and one uxto for collateral
admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json

cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')

cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace == '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-collateral-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-collateral-valid.json
admin_utxo_collateral_in=$(echo $admin_utxo_valid_array | tr -d '\n')


# Step 2: Get the littercoin smart contract which has the thread token
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
#echo -n "$lc_validator_datum_in" | jq 'to_entries[] | .value.inlineDatum' > $WORK/lc-datum-in.json
echo -n "$lc_validator_datum_in" > $WORK/lc-datum-in.json


# get the current total Ada value
total_ada=$(jq -r '.list[0].int' $WORK/lc-datum-in.json)
new_total_ada=$(($total_ada + $ada_amount))

# Update the littercoin datum accordingly
cat $WORK/lc-datum-in.json | \
jq -c '
  .list[0].int   |= '$new_total_ada'' > $WORK/lc-datum-out.json


# Upate the redeemer with the amount of add being added
#cat $redeemer_file_path | \
#jq -c '
#  .fields[0].int          |= '$ada_amount'' > $WORK/redeemer-add-ada.json


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
  --spending-reference-tx-in-redeemer-file "$redeemer_file_path" \
  --tx-out "$lc_validator_script_addr+$new_total_ada + 1 $thread_token_mph.$thread_token_name" \
  --tx-out-inline-datum-file "$WORK/lc-datum-out.json"  \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/add-ada-tx-alonzo.body

#  --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/add-ada.costs"
  

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/add-ada-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/add-ada-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/add-ada-tx-alonzo.tx $network



