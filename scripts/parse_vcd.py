import re


def get_vcd_signals(fname):
    stack=[]
    vars=[]

    with open(fname,"r") as fp:
        for line in fp.readlines():
            #print(line,stack)
            line=line.strip()

            x=re.match(r"\$var (?P<type>\S+) (?P<width>\d+) (?P<symbol>\S+) (?P<name>\S+)( (?P<range>)\S+)? \$end",line)
            if x:
                vars.append(tuple(stack+[x.group("name")]))
            
            x=re.match(r"\$scope (?P<type>\S+) (?P<name>\S+) \$end",line)
            if x:
                stack.append(x.group("name"))

            x=re.match(r"\$upscope \$end",line)
            if x:
                stack.pop()
                if not stack: #close TOP
                    break
    return vars

if __name__=="__main__":
    import os
    os.chdir(os.path.dirname(__file__))

    from pprint import pp
    pp(get_vcd_signals("waveform.vcd"))
