

import xml.etree.ElementTree as ET
from pprint import pp as pprint
from collections import namedtuple


#type Loc = tuple[str,int,int,int,int]
#class Loc(tuple):
#    pass
Loc = namedtuple("Loc",["file","l1","c1","l2","c2"])

class HierarchyElement:
    def lookup(self,path,root=None):
        # default to self as root
        if root is None:
            root=self
        
        # end of path
        if not path:
            return self
        
        return self._lookup(path,root)

class Variable(HierarchyElement):
    __match_args__=("name","loc")
    def __init__(self,name,loc,tag=None):
        self.name=name
        self.loc=loc
        self.tag=tag
        self.type=None
    def _lookup(self,path,_):
        path=".".join(path)
        raise KeyError(f"Variable {self.name} has no subelements")
    def __repr__(self):
        return "${self.name}"

class Scope(HierarchyElement):
    __match_args__=("name","children")
    def __init__(self,name):
        self.name=name
        self.children={}
    def __getitem__(self,item):
        return self.children[item]
    def __setitem__(self,item,value):
        self.children[item]=value
    def _lookup(self,path,root):
        return self[path[0]].lookup(path[1:],root)
    def __repr__(self):
        return f"({self.name})"

class ScopeRef(HierarchyElement):
    __match_args__ = ("name","module")
    def __init__(self,name,module):
        self.name=name
        self.module=module
    def _lookup(self,path,root):
        return root[self.module].lookup(path,root)
    def __repr__(self):
        return f"({self.name}->{self.module})"

def loc(str) -> Loc:
    file,r=str.split(",",1)
    return Loc(file,*map(int,r.split(",")))


def parse_hierarchy(el,scope: Scope) -> Scope:
    #print(el)
    if el.tag=="var":
        scope[el.attrib["name"]]=Variable(el.attrib["name"],loc(el.attrib["loc"]),tag=el.attrib.get("tag"))
    elif el.tag=="instance":
        scope[el.attrib["name"]]=ScopeRef(el.attrib["name"],el.attrib["defName"])
    elif el.tag in ("begin","module") and el.attrib.get("name"):
        name=el.attrib["name"]
        subscope=Scope(name)
        scope[name]=subscope
        for c in el:
            parse_hierarchy(c,subscope)
    else:
        for c in el:
            parse_hierarchy(c,scope)

    return scope

def parse_files(el) -> dict[str,str]:
    files={}
    for f in el:
        if f.tag=="file":
            files[f.attrib["id"]]=f.attrib["filename"]
        else:
            raise Exception("Unexpected files list structure")
    return files

def parse_xml(fname):
    tree=ET.parse(fname)
    root=tree.getroot()

    # parse source files
    files=root.find("files")
    files=parse_files(files)

    # parse netlist
    netlist=root.find("netlist")
    hierarchy=parse_hierarchy(netlist,Scope(None))

    return hierarchy, files


def printh(h: HierarchyElement,indent=0):
        print(" "*indent,end="")
        match h:
            case Scope(name,children):
                print(name)
                for c in children.values():
                    printh(c,indent+2)
            case Variable(name,loc):
                print(f"$ {name}\t@ {loc}")
            case ScopeRef(name,module):
                print(f"{name} -> {module}")

if __name__=="__main__":
    # if run separately, take the XML file as input and print the hierarchy

    import sys

    xml = sys.argv[1]
    hierarchy, files = parse_xml(xml)
    
    print("FILES:")
    for id,fname in files.items():
        print(id,fname,sep="\t")
    print()

    def printh(h: HierarchyElement,indent=0):
        print(" "*indent,end="")
        match h:
            case Scope(name,children):
                print(name)
                for c in children.values():
                    printh(c,indent+2)
            case Variable(name,loc):
                print(f"$ {name}\t@ {loc}")
            case ScopeRef(name,module):
                print(f"{name} -> {module}")
    
    print("HIERARCHY:")
    printh(hierarchy)