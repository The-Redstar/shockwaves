# run as command in terminal
import sys
import getopt
import json
from collections import defaultdict

from parse_xml import parse_xml, printh
from parse_vcd import get_vcd_signals, get_signal_values
from parse_types import parse_type
from match_signals import match_signals

opts,posopts=getopt.getopt(sys.argv[1:],"x:w:s:o:dv:a:h:",["xml=","vcd=","signals=","output=","haskell=","debug","verilog-dir=","values=","help"])
args={}

if len(posopts)>=1:
    args["output"]=posopts[-1]
    if len(posopts)>=2:
        args["xml"]=posopts[0]
        if len(posopts)>=3:
            args["vcd"]=posopts[1]

for k,v in opts:
    k=k.lstrip("-")
    if k in "kvsodah":
        args[dict(x="xml",w="vcd",s="signals",o="output",d="debug",v="verilog-dir",a="values",h="haskell")[k]]=v
    else:
        args[k]=v

if "help" in args:
    print("""
Extract type data from annotated verilog and an XML verilog description for selected signals.

Options:
    -x, --xml           XML file to use
    -w, --vcd           Extract signals from VCD file
    -s, --signals       Use list of signals, one per line (mutually exclusive with --vcd)
    -o, --output        Output file for signal matching
    -h, --haskell       Output file for Haskell type conversion table
    -v, --verilog-dir   Look for files listed in the XML file in this directory, rather than the current working directory
    -a, --values        Extract all values of each type and store them in this file
    -d, --debug         Print results to stdout
""")
    exit(0)

assert("xml" in args)
assert("vcd" in args or "signals" in args)
assert(not("vcd" in args and "signals" in args))
assert("vcd" in args or not "values" in args)

# XML, VCD, OUTPUT
hierarchy,files=parse_xml(args["xml"])
if "debug" in args:
    print("HIERARCHY:")
    printh(hierarchy)
    print("FILES:",files)

if "vcd" in args:
    signals,symbols=get_vcd_signals(args["vcd"])
else:
    with open(args["signals"],"r") as fp:
        signals=list(map(lambda s:tuple(s.strip.split(".")),fp.readlines()))
if "debug" in args:
    print("SIGNALS:",signals)

matching = match_signals(hierarchy,files,signals,verilog_dir=args.get("verilog-dir"))
if "debug" in args:
    print("MATCHED SIGNALS:",matching)

if "output" in args:
    with open(args["output"],"w") as fp:
        json.dump(matching,fp)

if "haskell" in args:
    types=list(set(matching.values()))
    imports=set()
    conversions=[]
    for ty in types:
        h,i=parse_type(ty)
        conversions.append((ty,h))
        imports.update(i)
    
    code=(
        "module Shockwaves where\n\n" +
        "import Prelude\n" +
        "import WaveForms.Translation(translateCmdLine,StructF,TransF,TypeFunctions(tf))\n\n" +
        "\n".join(f"import qualified {i}" for i in imports) +
        "\n\n" +
        "types :: String -> (StructF,TransF)\n" +
        "types tag = case tag of\n" +
        "\n".join(f"  {json.dumps(ty)} -> tf @({h})" for ty,h in conversions) +
        "\n\n" +
        "main = translateCmdLine types"
    )

    with open(args["haskell"], "w") as fp:
        fp.write(code)

if "values" in args and "vcd" in args:
    
    for s in symbols:
        symbols[s]=[".".join(v) for v in symbols[s] if ".".join(v) in matching]
    for k in list(symbols.keys()):
        if not symbols[k]:  del symbols[k]

    values=defaultdict(set)
    for sym,val in get_signal_values(args["vcd"]):
        match matching.get(".".join(symbols[sym])):
            case None: pass
            case ty:
                values[ty].add(val)
    
    with open(args["values"], "w") as fp:
        for ty,vals in values.items():
            fp.write(ty+"\n")
            fp.write(" ".join(vals)+"\n")
        


if "debug" in args:
    print("SIGNAL TYPES:")
    for sig,type in matching.items():
        print(sig,"::",type)