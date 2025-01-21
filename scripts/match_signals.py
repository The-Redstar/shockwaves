
from collections import defaultdict
import re
import os
import json

from parse_xml import parse_xml, Scope, Variable, Loc
from parse_vcd import get_vcd_signals, get_signal_values


TYPE_ANNOTATION_RE = re.compile(r"\s*/\*TYPE (.*)\*/")

def match_signals(hierarchy: Scope,files: dict[str,str],signals: list[tuple[str]],verilog_dir: str = None) -> dict[tuple[str],str]:

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
    
    wd=os.getcwd()
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

    os.chdir(wd)

    # assign types to signals
    matching: dict[tuple[str],str] = {} # variable to type

    for (sig,loc) in todo_vars:
        if loc in loc_to_type:
            matching[".".join(sig)]=loc_to_type[loc]

    return matching
