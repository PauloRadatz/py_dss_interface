# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface import DSS
import pathlib

dss = DSS()

dss_file = r"C:\eniocc\EPRI\py_dss_interface-master\src\py_dss_interface\tests\py_dss_interface\13Bus\IEEE13Nodeckt" \
           r".dss "


dss.text("compile {0}".format(dss_file))
overload_file_path = pathlib.Path(dss_file).parent.joinpath(f"{dss.circuit_name()}_EXP_OVERLOADS.CSV")

# Solve
# dss.text('export overloads')
# text.text('export overloads')
dss.text("? Load.611.kw")
dss.solution_solve()
# dss.text("solve")
dss.loadshapes_first()
dss.loadshapes_read_pmult()

#
new = list(dss.loadshapes_read_pmult())
new[2] = 0
# print(new)
#
dss.loadshapes_write_pmult(new)
# Show Voltage Report

# print(dss.loadshapes_read_pmult())
# dss.text.text("show voltages")

print("Resultado: {0}".format(dss.circuit_total_power()))
# print(dss.dssinterface.dss_read_datapath())
# Get all buses voltages
print(dss.circuit_all_bus_volts())

# from  py_dss_interface import Loads
