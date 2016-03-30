#!/bin/bash

source setup.sh

FILENAME=${1}
shift

# compile llvm code into bitcode
llvm-as-${LLVM_VERSION} <"${FILENAME}.ll" \
# optimize llvm registers
    | opt-${LLVM_VERSION} -mem2reg \
# disassemble back to llvm code to check the optimization
    | llvm-dis-${LLVM_VERSION} >"${FILENAME}-optimized.ll"
