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


dss = py_dss_interface.DSS()

dss.text("set DefaultBaseFrequency=60")
script_path = os.path.dirname(os.path.abspath(__file__))
dss13_path = os.path.join(pathlib.Path(script_path), "tests", "py_dss_interface", "cases", "13Bus", "IEEE13Nodeckt.dss")
dss.text(f"compile {dss13_path}")

dss.text("solve")

dss.circuit.set_active_element("transformer.sub")

print("here")
