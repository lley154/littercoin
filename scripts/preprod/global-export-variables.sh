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
export USER_VKEY=/home/lawrence/.local/keys/testnet/user/user.vkey
export USER_SKEY=/home/lawrence/.local/keys/testnet/user/user.skey
export USER_PKH=/home/lawrence/.local/keys/testnet/user/user.pkh
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=30000000
export COLLATERAL_ADA=5000000
#export LC_VAL_REF_SCRIPT=c5ce3dd5330ba04702afff35e31fd01cc6176b003bcb2a11515e29c402940ba5#2
#export LC_MINT_REF_SCRIPT=c5ce3dd5330ba04702afff35e31fd01cc6176b003bcb2a11515e29c402940ba5#3
#export MERCHANT_TOKEN_MPH=e57b84e97afe75117f906e57e66ca0718e25c9db3c4076f2bf78555b
#export MERCHANT_TOKEN_NAME=4d65726368616e7420546f6b656e204c6974746572636f696e
#export MERCHANT_TOKEN_MPH=a227d65452b79c4182c043080140322c65fac517b14e69e8bddf07d4
#export MERCHANT_TOKEN_NAME=4d65726368616e7420546f6b656e204c6974746572636f696e
export BECH32=/usr/local/bin/bech32
export ADDR_PREFIX=addr_test
export BECH32_NETWORK=00


