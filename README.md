[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![build](https://github.com/meelgroup/approxmc/workflows/build/badge.svg)


# ApproxMCCert: Formally Certified Approximate Model Counter
ApproxMCCert is a formally certified approximate model counter utilizing formal proof and a certification pipeline to give certified approximate model counts.

This work is by Yong Kiam Tan, Jiong Yang, Mate Soos, Magnus O. Myreen, and Kuldeep S. Meel, to appear in CAV24.

This repository includes the source code of ApproxMCCert, an approximate model counter with certificate generation, and CertCheck, the corresponding certificate checker.
The formal proof of the ApproxMC algorithm in [Isabelle](https://isabelle.in.tum.de/) is available on [Archive of Formal Proofs](https://isa-afp.org/entries/Approximate_Model_Counting.html).
CertCheck needs access to an external CNF-XOR UNSAT checker, where a CNF-XOR solver [CryptoMiniSat](https://github.com/msoos/cryptominisat) outputs the UNSAT proof that is translated and verified by [FRAT-xor and cake_xlrup](https://github.com/meelgroup/frat-xor).

If you are only interested in scalable approximate model counting, visit our state-of-the-art counter [ApproxMC](http://github.com/meelgroup/approxmc).


## How to Build a Binary
To build on Linux, you will need the following:
```bash
sudo apt-get install build-essential cmake
apt-get install libgmp3-dev
```

Then, build CryptoMiniSat, Arjun, and ApproxMC:
```bash
# not required but very useful
sudo apt-get install zlib1g-dev

git clone https://github.com/meelgroup/cadical
cd cadical
git checkout mate-only-libraries-1.8.0
./configure
make
cd ..

git clone https://github.com/meelgroup/cadiback
cd cadiback
git checkout mate
./configure
make
cd ..

git clone https://github.com/msoos/cryptominisat
cd cryptominisat
mkdir build && cd build
cmake ..
make
cd ../..

git clone https://github.com/meelgroup/sbva
cd sbva
mkdir build && cd build
cmake ..
make
cd ../..

git clone https://github.com/meelgroup/arjun
cd arjun
mkdir build && cd build
cmake ..
make
cd ../..

git clone https://github.com/meelgroup/approxmc
cd approxmc
mkdir build && cd build
cmake ..
make
sudo make install
sudo ldconfig
```

## How to Use the Binary
First, you must translate your problem to CNF and just pass your file as input to ApproxMC. Voila -- and it will print the number of solutions of the given CNF formula.

### Providing a Sampling Set
For some applications, one is not interested in solutions over all the variables and instead interested in counting the number of unique solutions to a subset of variables, called sampling set. ApproxMC allows you to specify the sampling set using the following modified version of DIMACS format:

```shell
$ cat myfile.cnf
c ind 1 3 4 6 7 8 10 0
p cnf 500 1
3 4 0
```
Above, using the `c ind` line, we declare that only variables 1, 3, 4, 6, 7, 8 and 10 form part of the sampling set out of the CNF's 500 variables `1,2...500`. This line must end with a 0. The solution that ApproxMC will be giving is essentially answering the question: how many different combination of settings to this variables are there that satisfy this problem? Naturally, if your sampling set only contains 7 variables, then the maximum number of solutions can only be at most 2^7 = 128. This is true even if your CNF has thousands of variables.

### Running ApproxMC
In our case, the maximum number of solutions could at most be 2^7=128, but our CNF should be restricting this. Let's see:

```shell
$ approxmc --seed 5 myfile.cnf
c ApproxMC version 3.0
[...]
c CryptoMiniSat SHA revision [...]
c Using code from 'When Boolean Satisfiability Meets Gauss-E. in a Simplex Way'
[...]
[appmc] using seed: 5
[appmc] Sampling set size: 7
[appmc] Sampling set: 1, 3, 4, 6, 7, 8, 10,
[appmc] Using start iteration 0
[appmc] [    0.00 ] bounded_sol_count looking for   73 solutions -- hashes active: 0
[appmc] [    0.01 ] bounded_sol_count looking for   73 solutions -- hashes active: 1
[appmc] [    0.01 ] bounded_sol_count looking for   73 solutions -- hashes active: 0
[...]
[appmc] FINISHED ApproxMC T: 0.04 s
c [appmc] Number of solutions is: 48*2**1
s mc 96
```
ApproxMC reports that we have approximately `96 (=48*2)` solutions to the CNF's independent support. This is because for variables 3 and 4 we have banned the `false,false` solution, so out of their 4 possible settings, one is banned. Therefore, we have `2^5 * (4-1) = 96` solutions.

### Guarantees
ApproxMC provides so-called "PAC", or Probably Approximately Correct, guarantees. In less fancy words, the system guarantees that the solution found is within a certain tolerance (called "epsilon") with a certain probability (called "delta"). The default tolerance and probability, i.e. epsilon and delta values, are set to 0.8 and 0.2, respectively. Both values are configurable.


## How to Cite
If you use ApproxMCCert, please cite our CAV24 paper.

If you use ApproxMC, please find the details [here](https://github.com/meelgroup/approxmc?tab=readme-ov-file#how-to-cite).

The benchmarks used in our evaluation can be found [here](https://zenodo.org/records/10449477).

