# -*- coding: utf-8 -*-
# @Time    : 7/11/2023 4:33 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : circuit_interface_PR.py
# @Software: PyCharm

import py_dss_interface

dss = py_dss_interface.DSS()

dss_file = r"C:\Program Files\OpenDSS\IEEETestCases\13Bus\IEEE13Nodeckt.dss"

dss.text(f"compile [{dss_file}]")
dss.text("new energymeter.m element=Transformer.Sub")
dss.text("solve")

# Int
num_nodes = dss.circuit.num_nodes
print(num_nodes)

# Float
capacity = dss.circuit.capacity
print(capacity)

# String
circuit_name = dss.circuit.name
print(circuit_name)

cktelement_active = dss.circuit.set_active_element("Transformer.Sub")
print(cktelement_active)
