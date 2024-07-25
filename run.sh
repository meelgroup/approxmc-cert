#! /bin/bash
set -e

# Limit each process to 16GB memory
ulimit -v 16000000

CNF=$1

# Generate random seed
./cert_tools/gen_rand 8//10 2//10 $CNF rand

# Call approxmc and generate certificate
./cert_tools/approxmc --arjun 0 --randbits rand --cert cert $CNF

# Run the certificate checker
./cert_tools/certcheck_cnf_xor 8//10 2//10 $CNF rand cert check_unsat_cms.sh
