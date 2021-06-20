# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import os
import pathlib
from py_dss_interface import DSSDLL as DSS

dss = DSS()

my_path = pathlib.Path(__file__).parents[1] # one level above
my_path = os.path.join(my_path, r"test\py_dss_interface\13Bus")
dss_file = os.path.join(my_path, "IEEE13Nodeckt.dss")

dss.text("compile {0}".format(dss_file))

overload_file_path = pathlib.Path(dss_file).parent.joinpath(f"{dss.circuit_name()}_EXP_OVERLOADS.CSV")

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
