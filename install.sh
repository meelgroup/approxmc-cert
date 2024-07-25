#! /bin/bash
set -e

# Install dependencies
apt install -y cmake cargo git zlib1g-dev libboost-program-options-dev \
    libboost-serialization-dev libmpfr-dev libgmp3-dev wget

python -m pip install lit

wget https://github.com/MLton/mlton/releases/download/on-20210117-release/mlton-20210117-1.amd64-linux-glibc2.31.tgz
tar -xf mlton-20210117-1.amd64-linux-glibc2.31.tgz
rm mlton-20210117-1.amd64-linux-glibc2.31.tgz
cd mlton-20210117-1.amd64-linux-glibc2.31/
make
cd ..
rm -rf mlton-20210117-1.amd64-linux-glibc2.31

# Install all the prerequisites into a folder cert_tools/
mkdir -p cert_tools

# Install cadical
git clone https://github.com/meelgroup/cadical
cd cadical
git checkout mate-only-libraries-1.8.0
./configure
make
cd ..

# Install cadiback
git clone https://github.com/meelgroup/cadiback
cd cadiback
git checkout mate
./configure
make
cd ..

# Install cryptominisat
git clone https://github.com/msoos/cryptominisat
cd cryptominisat
mkdir build && cd build
cmake ..
make
cp cryptominisat5 ../../cert_tools/cryptominisat5
cd ../..

# Install sbva
git clone https://github.com/meelgroup/sbva
cd sbva
mkdir build && cd build
cmake ..
make
cd ../..

# Install arjun
git clone https://github.com/meelgroup/arjun
cd arjun
mkdir build && cd build
cmake ..
make
cd ../..

# Install approxmc with certification
mkdir build && cd build
cmake ..
make
cp approxmc ../cert_tools/approxmc
cd ..

# Install frat-xor
git clone https://github.com/meelgroup/frat-xor.git
cd frat-xor
cargo clean
cargo build --release
cp target/release/frat-xor ../cert_tools/frat-xor


# Install cake_xlrup
cd cake_xlrup

# Try to use nearly 4 GB
gcc basis_ffi.c cake_xlrup.S -o cake_xlrup -static -std=c99 \
		-DCML_HEAP_SIZE=2000 -DCML_STACK_SIZE=2048
cp cake_xlrup ../../cert_tools/cake_xlrup
cd ../..

# Build random bit generator and CertCheck
cd certcheck
make
cp gen_rand ../cert_tools/gen_rand
cp certcheck_cnf_xor ../cert_tools/certcheck_cnf_xor
cd ..

# After this script, cert_tools contains:
# approxmc, cake_xlrup, cryptominisat5, certcheck_cnf_xor, frat-xor, and gen_rand.
