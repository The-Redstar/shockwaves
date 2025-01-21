
from collections import defaultdict
import re
import os
import json

from parse_xml import parse_xml, Scope, Variable, Loc
from parse_vcd import get_vcd_signals, get_signal_values
from parse_types import parse_type


TYPE_ANNOTATION_RE = re.compile(r"\s*/\*TYPE (.*)\*/")

def match_signals(hierarchy: Scope,files: dict[str,str],signals: list[tuple[str]],verilog_dir: str = None):

    # in the simulation, the actual signals are in scope TOP rather than the toplevel scope
    hierarchy.name="TOP"
    hierarchy_wrapped=Scope(None)
    hierarchy_wrapped["TOP"]=hierarchy

    todo_vars: list[(str,Loc)] = [] #variables to check
    todo_locs: set[Loc]  = set() #locations to check

    # lookup variable locations
    for sig in signals:
        try:
            v = hierarchy_wrapped.lookup(sig,hierarchy)
        except KeyError:
            pass # not found in design
        else:
            match v:
                case Variable(loc=loc):
                    #print(sig,"@",loc)
                    todo_locs.add(loc)
                    todo_vars.append((sig,loc))
                case _:
                    print("expected signal",".".join(sig),"found",v)
                    pass # not a variable
    
    # check locations of interest in files
    loc_to_type: dict[Loc,str] = {}

    locs=list(todo_locs)
    locs.sort(key=lambda t: (t[0],t[3],t[4])) #sort by file, then end point

    locs_by_file: defaultdict[str,list[Loc]]=defaultdict(list)
    for loc in locs:
        locs_by_file[loc[0]].append(loc)
    
    if verilog_dir: os.chdir(verilog_dir)

    for f,locs in locs_by_file.items():
        file=files[f]
        if file.startswith("<"):
            continue
        with open(file,"r") as fp:
            l_index=0
            for l,line in enumerate(fp,start=1):
                while l_index<len(locs) and locs[l_index].l2==l:
                    # look for type tag
                    print(locs[l_index],line[locs[l_index].c2-1:])
                    m = TYPE_ANNOTATION_RE.match(line[locs[l_index].c2-1:])
                    if m is not None:
                        loc_to_type[locs[l_index]]=m.group(1)

                    l_index+=1
                if l_index==len(locs):
                    break


    # assign types to signals
    matching: dict[tuple[str],str] = {} # variable to type

    for (sig,loc) in todo_vars:
        if loc in loc_to_type:
            matching[".".join(sig)]=loc_to_type[loc]

    return matching

if __name__=="__main__":
    # run as command in terminal
    import sys
    import getopt

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
    if "vcd" in args:
        signals,symbols=get_vcd_signals(args["vcd"])
    else:
        with open(args["signals"],"r") as fp:
            signals=list(map(lambda s:tuple(s.strip.split(".")),fp.readlines()))

    wd=os.getcwd()
    matching = match_signals(hierarchy,files,signals,verilog_dir=args.get("verilog-dir"))
    os.chdir(wd)

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
            "import WaveFormTranslateAll (translateCmdLine)\n\n" +
            "\n".join(f"import qualified {i}" for i in imports) +
            "\n" +
            "types :: String -> (StructF,TransF)\n" +
            "types tag = case tag of\n" +
            "\n".join(f"  {repr(ty)} -> tf @ ({h})" for ty,h in conversions) +
            "\n\nmain = translateCmdLine types"
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
            print(sig,type)
    
