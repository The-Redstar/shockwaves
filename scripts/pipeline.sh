#!/bin/sh

# Script containing full pipeline for debugging


# Compile Clash design
# clas design --> verilog files, type source information
# TODO

# Simulate using verilator
# verilog --> <waves>.vcd
# TODO

# Extract data using verilator
# verilog --> XML
# TODO

# Extract type and signal information
# verilog, XML --> <waves>.types.json
# TODO

# Generate Haskell code
# VCD, <waves>.types.json --> translator haskell code
# TODO

# Translate values using GHCI
# Clash design, translator code, Haskell library --> <waves>.trans.json
# TODO

# Run Surfer
# TODO
