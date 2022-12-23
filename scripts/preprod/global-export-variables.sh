#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

# Littercoin fixed supply 1 billion
export LC_SUPPLY=1000000000
export BASE=/config/workspace/repo
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/ipc/node.socket
export TESTNET_MAGIC=1
export ADMIN_VKEY=/config/.local/keys/key.vkey
export ADMIN_SKEY=/config/.local/keys/key.skey
export ADMIN_PKH=/config/.local/keys/key.pkh
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=30000000
export COLLATERAL_ADA=5000000

# Not needed for init-tx.sh
#export LC_VAL_REF_SCRIPT=
#export MERCHANT_TOKEN_MPH=
#export MERCHANT_TOKEN_NAME=





