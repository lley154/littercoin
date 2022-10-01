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
    echo "burn-littercoin-tx.sh:  Invalid script arguments"
    echo "Usage: burn-littercoin-tx.sh [devnet|testnet|mainnet]"
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
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-minting-policy.hash | jq -r '.bytes')
thread_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-name.json | jq -r '.bytes')
nft_mint_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/nft-minting-policy.hash | jq -r '.bytes')
nft_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/nft-token-name.json | jq -r '.bytes')
lc_mint_script="$BASE/scripts/cardano-cli/$ENV/data/lc-minting-policy.plutus"
lc_mint_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lc-minting-policy.hash | jq -r '.bytes')
lc_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lc-token-name.json | jq -r '.bytes')
lc_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lc-validator.plutus"
lc_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lc_validator_script" $network)
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-burn.json"
redeemer_lc_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-burn-lc.json"


admin_pkh=$(cat $ADMIN_PKH)
lc_amount=25
lc_remaining=50

# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for minting of the token
# and one uxto for collateral
admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json
cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')

# Pull the utxo with the merchant nft token
admin_nft_utxo_tx_in=$(jq -r 'to_entries[] 
| select(.value.value."'$nft_mint_mph'"."'$nft_token_name'") 
| .key' $WORK/admin-utxo.json)


# Pull the utxo with the littercoin tokens
admin_lc_utxo_tx_in=$(jq -r 'to_entries[] 
| select(.value.value."'$lc_mint_mph'"."'$lc_token_name'") 
| .key' $WORK/admin-utxo.json)


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


# Get the current total Ada and Littercoin amount in the smart contract
total_ada=$(jq -r '.fields[0].int' $WORK/lc-datum-in.json)
total_lc=$(jq -r '.fields[1].int' $WORK/lc-datum-in.json)

# Calculate the amount of Ada to withdraw
withdraw_ada=$((($total_ada / $total_lc) * $lc_amount))

new_total_ada=$(($total_ada - $withdraw_ada))
new_total_lc=$(($total_lc - $lc_amount))

# Update the littercoin datum accordingly
cat $WORK/lc-datum-in.json | \
jq -c '
  .fields[0].int   |= '$new_total_ada'
| .fields[1].int   |= '$new_total_lc'' > $WORK/lc-datum-out.json

# Upate the redeemer for the validator with the amount of littercoin being burned
cat $redeemer_file_path | \
jq -c '
  .fields[0].int          |= '$lc_amount'' > $WORK/redeemer-burn.json

# Update the redeemer for minting policy to indicate the amount of ada being spent
cat $redeemer_lc_file_path | \
jq -c '
  .fields[1].int          |= '$withdraw_ada'' > $WORK/redeemer-burn-lc.json



# Step 3: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --tx-in "$admin_utxo_in" \
  --tx-in "$admin_nft_utxo_tx_in" \
  --tx-in "$admin_lc_utxo_tx_in" \
  --tx-in "$lc_validator_utxo_tx_in" \
  --spending-tx-in-reference "$LC_VAL_REF_SCRIPT" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$WORK/redeemer-burn.json" \
  --mint "-$lc_amount $lc_mint_mph.$lc_token_name" \
  --mint-tx-in-reference "$LC_MINT_REF_SCRIPT" \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file "$WORK/redeemer-burn-lc.json" \
  --policy-id "$lc_mint_mph" \
  --tx-out "$lc_validator_script_addr+$new_total_ada + 1 $thread_token_mph.$thread_token_name" \
  --tx-out-inline-datum-file "$WORK/lc-datum-out.json"  \
  --tx-out "$admin_utxo_addr+$withdraw_ada + 1 $nft_mint_mph.$nft_token_name" \
  --tx-out "$admin_utxo_addr+$MIN_ADA_OUTPUT_TX + $lc_remaining $lc_mint_mph.$lc_token_name" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/burn-lc-tx-alonzo.body

#  --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/burn-littercoin.costs"

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/burn-lc-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/burn-lc-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/burn-lc-tx-alonzo.tx $network




