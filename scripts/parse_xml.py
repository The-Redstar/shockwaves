



import xml.etree.ElementTree as ET
from pprint import pp as pprint






def children(el):
    return [e for e in el.iter() if e is not el]

def find_signals(el,data):
    if el.tag=="var":
        data[el.attrib["name"]]=("var",dict(name=el.attrib["name"],tag=el.attrib.get("tag")))
    elif el.tag=="instance":
        data[el.attrib["name"]]=("module",dict(name=el.attrib["name"],module=el.attrib["defName"]))
    elif el.tag == "begin" and el.attrib.get("name"):
        d={}
        for c in children(el):
            find_signals(c,d)
        data[el.attrib["name"]]=d
    else:
        for c in children(el):
            find_signals(c,data)

    return data



def lookup(scopes,current_scope,path):
    key=path[0]
    path=path[1:]
    current_scope=current_scope[key]
    if not path:
        return current_scope
    try:
        current_scope[0]
    except:
        pass
    else:
        assert(current_scope[0]=="module") # can't look into var
        current_scope=scopes[current_scope[1]["module"]]
    return lookup(scopes,current_scope,path)


def get_scopes(fname):
    tree=ET.parse(fname)

    root=tree.getroot()

    netlist=root.find("netlist")
    scopes={}
    for child in children(netlist):
        if child.tag=="module":
            print("MODULE",child.attrib["name"])


            scopes[child.attrib["name"]]=find_signals(netlist,{})
    return scopes


if __name__=="__main__":
    import os
    os.chdir(os.path.dirname(__file__))

    scopes=get_scopes("obj_dir/Valu.xml")
    pprint(scopes)

    print(lookup(scopes,scopes,["alu","genblk1[3]","submodule_test","clk"]))


"""

proper way to do this (Haskell/Rust pattern matching)

data Name=String
data Tag=String
data Module=String

data ScopeElement = SubScope Name Scope | Var Name Tag  | ModuleRef Name Module
data Scope = Map Name ScopeElement

lookup global current [] = Just current
lookup global current (x:xs) = (\current' -> lookup global current' xs) <$> Map.lookup current x



for all scopes in a VCD:
parse VCDscope:
    lookup all vars; if they have annotated data, store that data somewhere
    recursively lookup scopes (append one to scope stack for every $scope, and pop for every $upscope)

"""