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
    echo "control-tx.sh:  Invalid script arguments"
    echo "Usage: control-tx.sh [devnet|preview|preprod|mainnet]"
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
validator_script="$BASE/scripts/cardano-cli/$ENV/data/validator.plutus"
validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$validator_script" $network)


# Get the littercoin smart contract with all utxos
$CARDANO_CLI query utxo --address $validator_script_addr $network --out-file $WORK/validator-utxo.json


jq -r 'to_entries[] 
| select(.value.value."'$DONOR_TOKEN_MPH'"."'$DONOR_TOKEN_NAME'") 
| .value.inlineDatum.fields[0].int, ",", .key, ",", "add-ada-tx.sh",">>>EOL" 
' /home/lawrence/src/littercoin/work/validator-utxo.json \
| tr -d '\n' | tr -s '>>>EOL' '\n' | tr -s ',' ' ' | tr -s '#' ' ' | sort -k 1 >> $WORK/action-utxo.txt


jq -r 'to_entries[] 
| select(.value.value."'$OWNER_TOKEN_MPH'"."'$OWNER_TOKEN_NAME'") 
| .value.inlineDatum.fields[0].int, ",", .key, ",", "mint-tx.sh",">>>EOL" 
' /home/lawrence/src/littercoin/work/validator-utxo.json \
| tr -d '\n' | tr -s '>>>EOL' '\n' | tr -s ',' ' ' | tr -s '#' ' ' | sort -k 1 >> $WORK/action-utxo.txt


jq -r 'to_entries[] 
| select(.value.value."'$MERCHANT_TOKEN_MPH'"."'$MERCHANT_TOKEN_NAME'") 
| .value.inlineDatum.fields[0].int, ",", .key, ",", "burn-tx.sh",">>>EOL" 
' /home/lawrence/src/littercoin/work/validator-utxo.json \
| tr -d '\n' | tr -s '>>>EOL' '\n' | tr -s ',' ' ' | tr -s '#' ' ' | sort -k 1 >> $WORK/action-utxo.txt



readarray actions < "$WORK/action-utxo.txt"

# If there are no action utxo at the validator script address, then gracefully exit
if !((${#actions[@]} > 0));
then
    exit 0
fi

                
row=${actions[@]}
row_array=(${row})                                                                                               
command=${row_array[3]}                                                   
utxo=${row_array[1]}
utxo_idx=${row_array[2]}

echo "command $command"
echo "utxo $utxo" 

# Execute the respective tx with the lowest sequence number
$BASE/scripts/cardano-cli/$command $ENV $utxo $utxo_idx









