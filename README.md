# Shockwaves

Shockwaves aims to make Haskell/Clash data types visible in the Surfer waveform viewer.

The most functional version of Shockwaves acts as a replacement of `Clash.Signal.Trace`, and works by generating a translation table from within Haskell. The appearance of data in the waveform viewer can be fully customized through custom implementations of the classes `Display` and `Split`.

Some flags are available for changing the default behaviour (i.e. using `ShowX` instead of `Show`, displaying `Left` as an error, etc.).

## Quick start

Requirements:
- A fork of [Surfer](https://gitlab.com/The-Redstar/surfer-shockwaves/) that has the Shockwaves translator.
- A project that uses `Clash.Signal.Trace`
- The Shockwaves library

Change the functions from `Clash.Signal.Trace` to those from `Shockwaves.Trace`.
Make sure all data types being traced derive `Show`, `Generic`, `Display` and `Split` (the latter half can be found in `Shockwaves` or `Shockwaves.Viewer`).
Change the code for storing the `VCD` output to the format shown in `Shockwaves.Trace`. Make sure to keep the filenames the same, apart from their file extension.
Finally, open the `VCD` file in the Surfer fork.

## Post-simulation translation

This repository contains scripts and code to extract type information from generated verilog, and pre-render signal values in the VCD.
This is all very experimental, but might be interesting to some.

The project depends on special version of [Clash](), which is currently not functional. A general description of the pipeline can be found below.


### Pipeline

| Step                                          | Performed by      | Input                             | Output                            |
| --------------------------------------------- | ----------------- | --------------------------------- | --------------------------------- |
| Compile with type comments                    | Clash compiler    | Clash design files                | Verilog files                     |
| List type source information                  | Clash compiler    | Clash design files                | `<type_sources>`                  |
| Extract verilog structure and sources         | Verilator         | Verilog files                     | <design>.xml                      |
| Simulate verilog                              | Verilator(?)      | Verilog files, simulation C++     | `<waves>.vcd`                     |
| Extract type data from Verilog and XML        | `parse_xml.py`, `run.py` | Verilog files, XML description | (runtime)           |
| Match VCD signals to types                    | `match_signals.py`, `run.py` | `<waves>.vcd`|signals list, type data | `<waves>.types.json`         |
| Generate Haskell imports                      | (python)          | `<type_sources>`                  | `<translate>.hs`                  |
| Generate Haskell type table                   | (python)          | Signal type data                  | `<translate>.hs`                  |
| Generate list of values to translate          | (python)          | VCD, `<waves.types.json>          | `<to_translate>`                  |
| Translate signals into                        | GHCI              | Clash design files, Haskell WaveForm library, `<translate>.hs`, `<to_translate>` | `<waves>.types.json`, `<waves>.translation.json` |
| Display Haskell types on screen               | surfer-shockwaves | `<waves>.vcd`,`<waves>.types.json`, `<waves>.translation.json` |      |