# ApproxMCCert: Formally Certified Approximate Model Counter
ApproxMCCert is a formally certified approximate model counter utilizing formal proof and a certification pipeline to give certified approximate model counts.

This work is by Yong Kiam Tan, Jiong Yang, Mate Soos, Magnus O. Myreen, and Kuldeep S. Meel, to appear in CAV24.

This repository includes the source code of ApproxMCCert, an approximate model counter with certificate generation, and CertCheck, the corresponding certificate checker.
The formal proof of the ApproxMC algorithm in [Isabelle](https://isabelle.in.tum.de/) is available on [Archive of Formal Proofs](https://isa-afp.org/entries/Approximate_Model_Counting.html).
CertCheck needs access to an external CNF-XOR UNSAT checker, where a CNF-XOR solver [CryptoMiniSat](https://github.com/msoos/cryptominisat) outputs the UNSAT proof that is translated and verified by [FRAT-xor and cake_xlrup](https://github.com/meelgroup/frat-xor).

If you are only interested in scalable approximate model counting, visit our state-of-the-art counter [ApproxMC](http://github.com/meelgroup/approxmc).
