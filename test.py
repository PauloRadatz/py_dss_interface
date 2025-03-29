# -*- coding: utf-8 -*-
# @Time    : 11/15/2021 10:00 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : line_example.py
# @Software: PyCharm

import py_dss_interface
import os
import pathlib

script_path = os.path.dirname(os.path.abspath(__file__))

dss_file = pathlib.Path(script_path).joinpath("tests",
                                              "py_dss_interface",
                                              "cases",
                                              "4Bus-YY-Bal",
                                              "4Bus-YY-Bal.DSS")

dss = py_dss_interface.DSS()
dss.text(f"compile [{dss_file}]")
dss.text("solve")

print(f"Total Powers : {dss.circuit.total_power}")
