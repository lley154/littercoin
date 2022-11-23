#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/littercoin
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/preprod/node.socket
export TESTNET_MAGIC=1
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/key.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/key.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/key.pkh
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=25000000
export COLLATERAL_ADA=5000000
export LC_VAL_REF_SCRIPT=0f17b0d0ae070a17b02362f661c106d59d7dfeec8278e40e601699ffe47fe8a6#2
export LC_MINT_REF_SCRIPT=0f17b0d0ae070a17b02362f661c106d59d7dfeec8278e40e601699ffe47fe8a6#3
export OWNER_TOKEN_MPH=9842d1c0c35c346399c0b18529bd469e30b46c000297c9b932bf151d
export OWNER_TOKEN_NAME=4f776e657220546f6b656e204c6974746572636f696e


