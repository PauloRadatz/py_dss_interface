# -*- coding: utf-8 -*-
# @Time    : 11/23/2022 1:13 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : script_tests.py
# @Software: PyCharm

import py_dss_interface
import pathlib
import numpy as np
import os

dss = py_dss_interface.DSSDLL()
dss = py_dss_interface.DSS()

dss.text("set DefaultBaseFrequency=60")
script_path = os.path.dirname(os.path.abspath(__file__))
dss13_path = os.path.join(pathlib.Path(script_path), "tests", "py_dss_interface", "cases", "13Bus", "IEEE13Nodeckt.dss")
dss.text(f"compile {dss13_path}")

dss.text(f"batchedit load..* daily=default")
dss.text("set mode=daily")
dss.text("New monitor.m1 element=Transformer.XFM1 terminal=1 mode=0")
dss.text("Set maxcontrol=100")
dss.text("Set maxiterations=100")
dss.text("Set controlmode=off")
dss.text("Set mode=daily stepsize=1.0h number=24")


dss.cktelement.bus_names = ["A", "B"]

dss.circuit.set_active_element("Transformer.XFM1")
print(dss.cktelement.name)
a = dss.cktelement.voltages_mag_ang
b = dss.cktelement.currents_mag_ang

dss.solution.solve()

dss.monitors.first()

actual = dss.monitors.channel(1)
