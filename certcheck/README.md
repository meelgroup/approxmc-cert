# CertCheck (CNF-XOR)

This folder contains the sources for CertCheck specialized for CNF-XOR approximate model counting.

The main file `certcheck_cnf_xor.ML` was exported from `Isabelle2024` from the corresponding AFP entry.

# Build Instructions

To build `certcheck_cnf_xor`, ensure that you have a working copy of the [MLton compiler](http://mlton.org/), then run `make` in this directory.

## Additional Dependencies

The tool requires access to a (verified) CNF-XOR unsatisfiability checking pipeline. A default pipeline script `check_unsat_cms.sh` is provided which assumes access to `cryptominisat`, `frat-xor`, and `cake_xlrup` placed in the `cert_tools` subfolder. These tools can be built and copied from the [CryptoMiniSat repository](https://github.com/msoos/cryptominisat) and the [frat-xor repository](https://github.com/meelgroup/frat-xor), respectively.

# Usage Instructions

First, the `gen_rand` tool can be used to generate enough random bits for a given input formula, e.g.:

```
gen_rand 8//10 2//10 ../example.cnf example.rand

c using eps: 4/5
c using del: 1/5
c projection set length: 10
c iters: 9
c Generating random bits: 891
```

Next, use `approxmc` (from this forked repository) with certification enabled to generate a certificate file:

```
approxmc --arjun 0 --randbits example.rand --cert example.cert ../example.cnf
```

Finally, run `certcheck_cnf_xor` on the certificate with access to the unsatisfiability checker.

```
certcheck_cnf_xor 8//10 2//10 ../example.cnf example.rand example.cert check_unsat_cms.sh
```

This should produce output similar to the following:

```
c using eps: 4/5
c using del: 1/5
c projection set length: 10
c iters: 9
c thresh: 73
c read rand bits: 896
c using UNSAT checker: check_unsat_cms.sh
c blast to CNF: false
c dumping CNF-XOR to file: ../example.cnf_approxmc_temp.xnf
c and calling UNSAT checker: check_unsat_cms.sh
c calling cryptominisat5
c calling frat-xor
c calling cake_xlrup
c shutdown UNSAT checker
...
s mc 184
```

# Partial Certificate Format

The partial certificate format expected by `certcheck_cnf_xor` is as follows.
Note that we call it a "partial" certificate because the unsatisfiability proofs are not included, but are rather checked by an external (verified) pipeline.

## First round

The first (0-th) round is always present. It starts with `0` followed by `n` indicating the number of distinct projected solutions of the input formula.
The next `n` lines must consist of satisfying solutions which are distinct after projection.

```
0
n
SOL_1
SOL_2
...
SOL_n
```

If `n < thresh`, then the formula is checked for unsatisfiability after banning `SOL_1` to `SOL_n`.
If this check is successful, the certificate is valid. Otherwise, the algorithm continues to its randomized part.

## Subsequent rounds

For all subsequent rounds (up to the number of rounds required for success amplication), the first number `1 <= m <= |S|-1` is the number of hashes that need to be added according to the approximation algorithm.

The check for correctness of `m` is in two parts, as justified in the following lines of the certificate.

```
m
n
SOL_1
SOL_2
...
SOL_n
-- additional lines when m < |S|-1
n'    
SOL_1
SOL_2
...
SOL_n'
-- end additional lines
```

### Checking m-1

The first part checks that `m-1` is insufficient, i.e., after adding `m-1` hashes, there are still too many solutions.
This follows a format similar to the above, i.e., a number `n` such that `n >= thresh` and a corresponding list of distinct projected solutions.

### Checking m

If `m < |S|-1`, then we also need to check that after adding `m` hashes, the number of solutions indeed falls below `thresh`.
This is indicated by an additional lines where, after adding `n'` solutions, the formula is checked for unsatisfiability.

## Putting it together

The final file is obtained by concatenating the output from all the rounds together. An example is in `example.cert`.

