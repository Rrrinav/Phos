#! /usr/bin/env bash

set -x
g++ -o ./main2 $1 ./bin/parser/ast-printer.o ./bin/parser/parser.o ./bin/type-checker/type-checker.o ./bin/utility/utility.o ./bin/value/type.o ./bin/value/value.o ./bin/vm/opcodes.o ./bin/vm/chunk.o ./bin/vm/virtual_machine.o ./bin/vm/compiler.o --std=c++23 -pthread -ggdb -O0
