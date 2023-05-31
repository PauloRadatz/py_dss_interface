# -*- coding: utf-8 -*-
# @Time    : 3/10/2023 10:40 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : script_version_2.py
# @Software: PyCharm

import py_dss_interface
import pathlib
import numpy as np
import os

dss = py_dss_interface.DSS("C:\OpenDSS_rep\Version8\Source")

dss.text("set DefaultBaseFrequency=60")
script_path = os.path.dirname(os.path.abspath(__file__))
dss13_path = os.path.join(pathlib.Path(script_path), "tests", "py_dss_interface", "cases", "13Bus", "IEEE13Nodeckt.dss")
dss.text(f"compile {dss13_path}")

# dss.text("set maxcontroli=1")
#
# dss.dssinterface.allow_forms = 0


dss.text("solve")

# dss.lines.first()
# line = dss.lines.names

dss.capacitors.first()
dss.capacitors.states = [1]
# dss.capacitors.states = [0, 1, 0, 1, 1]
#
dss.capacitors.names
# print(dss.text(f"? capacitor.{dss.capacitors.name}.states"))
# dss.capacitors.states
dss.capacitors.num_steps = 5
dss.capacitors.add_step()
dss.capacitors.add_step()
dss.capacitors.add_step()
dss.capacitors.subtract_step()
dss.capacitors.subtract_step()
dss.capacitors.states


print("here")
# if dss.solution.control_iterations == dss.solution.max_control_iterations:
#     print("Scenario with Max Control Iteration Issue")
#
# voltage_results = list()
# for hour in range(24):
#     dss.text("solve")
#
#     # dss.generators.first()
#     dss.circuit.set_active_element("generator.g")
#     v = dss.cktelement.voltages_mag_ang[0:6:2]
#
#     if v < 0.95:
#         dss.text("edit generation.g XRdp=1e12 Xdp=0.3 Xdpp=0.25 H=3.5 D=0")
