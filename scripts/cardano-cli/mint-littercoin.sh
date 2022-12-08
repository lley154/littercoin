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
thread_token_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-minting-policy.hash | jq -r '.bytes')
thread_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/thread-token-name.json | jq -r '.bytes')
lc_mint_script="$BASE/scripts/cardano-cli/$ENV/data/lc-minting-policy.plutus"
lc_mint_mph=$(cat $BASE/scripts/cardano-cli/$ENV/data/lc-minting-policy.hash | jq -r '.bytes')
lc_token_name=$(cat $BASE/scripts/cardano-cli/$ENV/data/lc-token-name.json | jq -r '.bytes')
lc_validator_script="$BASE/scripts/cardano-cli/$ENV/data/lc-validator.plutus"
lc_validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$lc_validator_script" $network)
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-mint.json"
redeemer_lc_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-mint-lc.json"
token_metadata_file_path="$BASE/scripts/cardano-cli/$ENV/data/lc-token-metadata.json"

admin_pkh=$(cat $ADMIN_PKH)

# read in littercoin batch file for processing
readarray rows < "$BASE/scripts/cardano-cli/$ENV/data/lc-minting-batch-file.txt"

lc_amount=0
for row in "${rows[@]}";do                                                      
  row_array=(${row})                                                            
  qty=${row_array[2]}                                                   
  lc_amount=$(($lc_amount + ${qty}))
done


# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for minting of the token
# and one uxto for collateral
if [ "$ENV" == "devnet" ];
then
    admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
    $CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json
    cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
    readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
    admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')
else
    admin_utxo_in=$ADMIN_UTXO
    admin_utxo_addr=$ADMIN_CHANGE_ADDR
fi


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


# get the current total Ada and Littercoin amount in the smart contract
total_ada=$(jq -r '.fields[0].int' $WORK/lc-datum-in.json)
total_lc=$(jq -r '.fields[1].int' $WORK/lc-datum-in.json)
new_total_lc=$(($total_lc + $lc_amount))

# Update the littercoin datum accordingly
cat $WORK/lc-datum-in.json | \
jq -c '
  .fields[1].int   |= '$new_total_lc'' > $WORK/lc-datum-out.json


# Upate the redeemer for the validator with the amount of littercoin being minted
cat $redeemer_file_path | \
jq -c '
  .fields[0].int          |= '$lc_amount'' > $WORK/redeemer-mint.json

# Update the redeemer for minting policy to indicate the amount of ada being spent
cat $redeemer_lc_file_path | \
jq -c '
  .fields[1].int          |= '$total_ada'' > $WORK/redeemer-mint-lc.json


# Step 3: Build and submit the transaction
printf '%s' "$CARDANO_CLI transaction build " > $WORK/cli-build.out
printf '%s' "--babbage-era " >> $WORK/cli-build.out
printf '%s' "--cardano-mode " >> $WORK/cli-build.out
printf '%s' "$network "  >> $WORK/cli-build.out
printf '%s'  "--change-address $admin_utxo_addr " >> $WORK/cli-build.out
printf '%s'  "--tx-in-collateral $ADMIN_COLLATERAL " >> $WORK/cli-build.out
printf '%s'  "--tx-in $admin_utxo_in " >> $WORK/cli-build.out
printf '%s'  "--tx-in $lc_validator_utxo_tx_in " >> $WORK/cli-build.out
printf '%s'  "--spending-tx-in-reference $LC_VAL_REF_SCRIPT " >> $WORK/cli-build.out
printf '%s'  "--spending-plutus-script-v2 " >> $WORK/cli-build.out
printf '%s'  "--spending-reference-tx-in-inline-datum-present " >> $WORK/cli-build.out
printf '%s'  "--spending-reference-tx-in-redeemer-file $WORK/redeemer-mint.json " >> $WORK/cli-build.out
printf '%s'  "--mint '$lc_amount $lc_mint_mph.$lc_token_name' " >> $WORK/cli-build.out
printf '%s'  "--mint-tx-in-reference $LC_MINT_REF_SCRIPT " >> $WORK/cli-build.out
printf '%s'  "--mint-plutus-script-v2 " >> $WORK/cli-build.out
printf '%s'  "--mint-reference-tx-in-redeemer-file $WORK/redeemer-mint-lc.json " >> $WORK/cli-build.out
printf '%s'  "--policy-id $lc_mint_mph " >> $WORK/cli-build.out
printf '%s'  "--tx-out '$lc_validator_script_addr+$total_ada + 1 $thread_token_mph.$thread_token_name' " >> $WORK/cli-build.out
printf '%s'  "--tx-out-inline-datum-file $WORK/lc-datum-out.json " >> $WORK/cli-build.out
for row in "${rows[@]}";do
  printf '%s' "--tx-out '$row' " | tr -d '\n' >> $WORK/cli-build.out                                                         
done
printf '%s'  "--required-signer-hash $admin_pkh " >> $WORK/cli-build.out
printf '%s'  "--protocol-params-file $WORK/pparms.json " >> $WORK/cli-build.out
printf '%s'  "--metadata-json-file $token_metadata_file_path " >> $WORK/cli-build.out
printf '%s'  "--calculate-plutus-script-cost $BASE/scripts/cardano-cli/$ENV/data/mint-littercoin.costs " >> $WORK/cli-build.out
#printf '%s'  "--out-file $WORK/mint-lc-tx-alonzo.body " >> $WORK/cli-build.out

bash $WORK/cli-build.out

#  --calculate-plutus-script-cost "$BASE/scripts/cardano-cli/$ENV/data/mint-littercoin.costs"

echo "tx has been built"

#$CARDANO_CLI transaction sign \
#  --tx-body-file $WORK/mint-lc-tx-alonzo.body \
#  $network \
#  --signing-key-file "${ADMIN_SKEY}" \
#  --out-file $WORK/mint-lc-tx-alonzo.tx

#echo "tx has been signed"

#echo "Submit the tx with plutus script and wait 5 seconds..."
#$CARDANO_CLI transaction submit --tx-file $WORK/mint-lc-tx-alonzo.tx $network
