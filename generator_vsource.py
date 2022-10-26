# -*- coding: utf-8 -*-
# @Time    : 6/27/2022 3:51 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : generator_vsource.py
# @Software: PyCharm

import py_dss_interface

dss = py_dss_interface.DSSDLL()
dss.text("ClearAll")
dss.text("New Circuit.Thevenin bus1=SourceBus phases=3 pu=1.0 basekv=13.8 model=ideal")
dss.text("New Vsource.S bus1=SourceBus phases=3 pu=1.0 basekv=13.8 model=ideal")
dss.text(f"New generator.G phases=3 bus1=SourceBus kv=13.8 kw=800 kvar=600")

dss.text("solve")

v_source = dss.vsources_all_names()
g = dss.generators_all_names()

print("here")
