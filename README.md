# ApproxMCCert: Formally Certified Approximate Model Counter
ApproxMCCert is a formally certified approximate model counter utilizing formal proof and a certification pipeline to give certified approximate model counts.

This repository includes the source code of ApproxMCCert, an approximate model counter with certificate generation, and CertCheck, the corresponding formally verified certificate checker.

The formal proof of the ApproxMC algorithm in [Isabelle](https://isabelle.in.tum.de/) is available on [Archive of Formal Proofs](https://isa-afp.org/entries/Approximate_Model_Counting.html).

CertCheck needs access to an external CNF-XOR UNSAT checker, where a CNF-XOR solver [CryptoMiniSat](https://github.com/msoos/cryptominisat) outputs the UNSAT proof that is translated and verified by [FRAT-xor and cake_xlrup](https://github.com/meelgroup/frat-xor).

If you are only interested in scalable approximate model counting, visit our state-of-the-art counter [ApproxMC](http://github.com/meelgroup/approxmc).

## Build
Run the following script to install all dependencies.

```bash
./install.sh
```

## Usage
The following script runs ApproxMCCert and outputs a certified approximate count or raises an error.

```bash
./run.sh cnf_file
```

You can find an example below.

```bash
./run.sh ./example.cnf
```

# References

- Formally certified approximate model counting is described in our [CAV'24 paper](https://www.cs.toronto.edu/~meel/publications/tysmm24.bib).

- Extension with support for BNN constraints is in our SAT'25 paper (to appear).
