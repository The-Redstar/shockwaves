# Shockwaves

Shockwaves aims to make Haskell/Clash data types visible in the Surfer waveform viewer. This repository contains scripts and code to extract type information from generated verilog, and pre-render signal values in the VCD.

The project depends on special version of [Clash]() and [Surfer](https://gitlab.com/The-Redstar/surfer-shockwaves/):


## Pipeline

| Step                                          | Performed by      | Input                             | Output                            |
| --------------------------------------------- | ----------------- | --------------------------------- | --------------------------------- |
| Compile with type comments                    | Clash compiler    | Clash design files                | Verilog files                     |
| List type source information                  | Clash compiler    | Clash design files                | `<type_sources>`                  |
| Extract verilog structure and sources         | Verilator         | Verilog files                     | <design>.xml                      |
| Simulate verilog                              | Verilator(?)      | Verilog files, simulation C++     | `<waves>.vcd`                     |
| Extract type data from Verilog and XML        | (python)          | Verilog files, XML description    | (runtime)                         |
| Match VCD signals to types                    | (python)          | `<waves>.vcd`, type data[, signals list]    | `<waves>.types.json`    |
| Generate Haskell imports                      | (python)          | `<type_sources>`                  | `<translate>.hs`                  |
| Generate Haskell type table                   | (python)          | Signal type data                  | `<translate>.hs`                  |
| Generate list of values to translate          | (python)          | VCD, `<waves.types.json>          | `<to_translate>`                  |
| Translate signals into                        | GHCI              | Clash design files, Haskell WaveForm library, `<translate>.hs`, `<to_translate>`     | `<waves>.types.json`, `<waves>.translation.json` |
| Display Haskell types on screen               | surfer-shockwaves | `<waves>.vcd`,`<waves>.types.json`, `<waves>.translation.json` |      |