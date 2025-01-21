import re
from collections import defaultdict


def get_vcd_signals(fname):
    stack:list[str]=[]
    vars:list[tuple[str]]=[]
    var_symbols:dict[str,list[tuple[str]]]=defaultdict(list)

    with open(fname,"r") as fp:
        for line in fp.readlines():
            #print(line,stack)
            line=line.strip()

            x=re.match(r"\$var (?P<type>\S+) (?P<width>\d+) (?P<symbol>\S+) (?P<name>\S+)( (?P<range>)\S+)? \$end",line)
            if x:
                sig=tuple(stack+[x.group("name")])
                vars.append(sig)
                var_symbols[x.group("symbol")].append(sig)
            
            x=re.match(r"\$scope (?P<type>\S+) (?P<name>\S+) \$end",line)
            if x:
                stack.append(x.group("name"))

            x=re.match(r"\$upscope \$end",line)
            if x:
                stack.pop()
                if not stack: #close TOP
                    break
    return vars,var_symbols

def get_signal_values(fname):
    with open(fname,"r") as fp:
        for line in fp.readlines():
            line=line.strip()
            if line:
                if line.startswith("$") or line.startswith("#"):
                    continue
                if line.startswith("b"):
                    v,s=line.split()
                    v=v[1:]
                    yield (s,v)
                else:
                    yield (line[1:],line[0])


if __name__=="__main__":
    import os
    os.chdir(os.path.dirname(__file__))

    from pprint import pp
    pp(get_vcd_signals("waveform.vcd"))
