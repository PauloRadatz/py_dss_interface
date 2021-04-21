# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
# First import the Package
from py_dss_interface import DSS
import pathlib

# Creates an OpenDSS object
dss = DSS()

# Select the DSS model
dss_file = r"C:\eniocc\EPRI\py_dss_interface-master\src\py_dss_interface\tests\py_dss_interface\13Bus\IEEE13Nodeckt" \
           r".dss "

# Compile
dss.Text.text("compile {}".format(dss_file))
overload_file_path = pathlib.Path(dss_file).parent.joinpath(f"{dss.Circuit.name()}_EXP_OVERLOADS.CSV")

# Solve
dss.Text.text('export overloads')
# text.text('export overloads')
dss.Text.text("? Load.611.kw")
dss.Solution.solution_solve()

dss.LoadShapes.loadshapes_first()
dss.LoadShapes.loadshapes_read_pmult()

new = list(dss.LoadShapes.loadshapes_read_pmult())
new[2] = 0

dss.LoadShapes.loadshapes_write_pmult(new)
# Show Voltage Report

print(dss.LoadShapes.loadshapes_read_pmult())
# dss.text.text("show voltages")

# print(dss.dssinterface.dss_read_datapath())
# Get all buses voltages
allbusvolts = dss.Circuit.all_bus_volts()

# print(dss.circuit.circuit_allbusvolts())
