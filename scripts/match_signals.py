
import os
os.chdir(os.path.dirname(__file__))

from parse_xml import lookup, get_scopes
from parse_vcd import get_vcd_signals

signals=get_vcd_signals("waveform.vcd")
scopes=get_scopes("obj_dir/Valu.xml")

for var in signals:
    try:
        x = lookup(scopes,dict(TOP=scopes),var)
    except:
        pass#print("FAILED",var)
    else:
        assert(x[0]=="var")
        print(var,x[1]["tag"])