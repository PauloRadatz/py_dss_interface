# First import the Package
import py_dss_interface
import pathlib
import os
# Creates an OpenDSS object
dss = py_dss_interface.DSSDLL()

# Select the DSS model
dss_file = r"C:\MeuTCC\Paulo_Example\DSSFiles\MASTER_RedeTeste13Barras.dss"

# Compile
dss.text("compile {}".format(dss_file))
overload_file_path = pathlib.Path(dss_file).parent.joinpath(f"{dss.circuit_name()}_EXP_OVERLOADS.CSV")

# Solve
dss.text('export overloads')
dss.text("? Load.611.kw")
dss.solution_solve()

dss.loadshapes_first()
dss.loadshapes_read_pmult()

new = list(dss.loadshapes_read_pmult())
new[2] = 0

dss.loadshapes_write_pmult(new)
# Show Voltage Report

print("worked here")
print(dss.loadshapes_read_pmult())
dss.text("show voltages")

print(dss.dss_read_datapath())
# Get all buses voltages
allbusvolts = dss.circuit_allbusvolts()

print(dss.circuit_allbusvolts())

