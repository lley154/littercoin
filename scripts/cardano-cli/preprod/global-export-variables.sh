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
export MIN_ADA_OUTPUT_TX=5000000
export MIN_ADA_OUTPUT_TX_REF=30000000
export COLLATERAL_ADA=5000000
export LC_VAL_REF_SCRIPT=d4ab97af6d703695eb191ca68d93d63333dafd3837753e189054b168aa15bc8b#2
export LC_MINT_REF_SCRIPT=d4ab97af6d703695eb191ca68d93d63333dafd3837753e189054b168aa15bc8b#3
export OWNER_TOKEN_MPH=66e10f73639b97d976275fe11778fadab379c1bf7cedd4dfb4219d9b
export OWNER_TOKEN_NAME=4f776e657220546f6b656e204c6974746572636f696e
export DONOR_TOKEN_MPH=d17c72f434ff89d37a1b1ee88721ae5aa0809eaafd1ac3d1df248794
export DONOR_TOKEN_NAME=446f6e6174696f6e204c6974746572636f696e
export MERCHANT_TOKEN_MPH=66e10f73639b97d976275fe11778fadab379c1bf7cedd4dfb4219d9b
export MERCHANT_TOKEN_NAME=4d65726368616e7420546f6b656e204c6974746572636f696e
export BECH32=/usr/local/bin/bech32
export ADDR_PREFIX=addr_test
export BECH32_NETWORK=00


