#! /bin/bash
set -e

if [ "$#" -ne 1 ]; then
    echo "UNSAT check using CryptoMiniSAT"
    echo "usage: check_unsat foo.xnf"
else

CNF=$1

tempxfrat=$(mktemp)
tempxlrup=$(mktemp)

# Call CryptoMiniSAT
set +e

echo "c calling cryptominisat5" >&2

# Generate FRAT-XOR proof
./cert_tools/cryptominisat5 --verbstat 0 $CNF $tempxfrat > /dev/null

if [ $? -ne 20 ];
then
  exit 1
fi
set -e

# Call frat-rs elaborator
echo "c calling frat-xor" >&2

# Translate FRAT-XOR to XLRUP proof
./cert_tools/frat-xor elab $tempxfrat $CNF $tempxlrup > /dev/null

# Call cake_xlrup verified proof checker
echo "c calling cake_xlrup" >&2

OUTPUT=$(./cert_tools/cake_xlrup $CNF $tempxlrup)

if echo $OUTPUT | grep -q "s VERIFIED UNSAT";
then
  echo "SUCCESS"
fi

fi
