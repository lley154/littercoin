#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/littercoin
export WORK=$BASE/work
export CARDANO_CLI=/home/lawrence/.local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/src/cardano-node/run/current/node-0/node.socket
export TESTNET_MAGIC=42
export GEN_VKEY=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo1.vkey
export GEN_SKEY=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo1.skey
export GEN_PKH=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo1.pkh
export ADMIN_VKEY=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo2.vkey
export ADMIN_SKEY=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo2.skey
export ADMIN_PKH=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo2.pkh
export USER_VKEY=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo3.vkey
export USER_SKEY=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo3.skey
export USER_PKH=/home/lawrence/.cache/cardano-workbench/genesis/k6-0.006kD-0kU-0a2ce2c/utxo-keys/utxo3.pkh
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=11000000
export ADMIN_COLLATERAL=52ff8bc4279a68dced5b1b6b86953f080bf162dd479d52b5cc5952e56d8e4fc4#2
export USER_COLLATERAL=
export LC_VAL_REF_SCRIPT=0c4cbbfba174517b62ad6e32d3c594033943143017182a6fb5647c7ae486f409#2
export LC_MINT_REF_SCRIPT=0c4cbbfba174517b62ad6e32d3c594033943143017182a6fb5647c7ae486f409#3
export NFT_MINT_REF_SCRIPT=0c4cbbfba174517b62ad6e32d3c594033943143017182a6fb5647c7ae486f409#4


