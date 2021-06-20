# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 11/10/2020
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# overload_file_path = pathlib.Path(dss_file).parent.joinpath(f"{dss.circuit_name()}_EXP_OVERLOADS.CSV")

# Solve
# dss.text('export overloads')
# text.text('export overloads')
dss.text("? Load.611.kw")
dss.solution_solve()
# dss.text("solve")
dss.loadshapes_first()
dss.loadshapes_read_p_mult()

#
new = list(dss.loadshapes_read_p_mult())
new[2] = 0
# print(new)
#
dss.loadshapes_write_p_mult(new)
# Show Voltage Report

# print(dss.loadshapes_read_pmult())
# dss.text.text("show voltages")

print("Resultado: {0}".format(dss.circuit_total_power()))
# print(dss.dssinterface.dss_read_datapath())
# Get all buses voltages
print(dss.circuit_all_bus_volts())

# from  py_dss_interface import Loads
