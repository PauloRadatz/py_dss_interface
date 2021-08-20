# First import the Package
import py_dss_interface
import pathlib
import os
# Creates an OpenDSS object
dss = py_dss_interface.DSSDLL()

# If specific DLL Version, use this line below
# dss = py_dss_interface.DSSDLL(dll_folder_param=r"C:\Users\eniocc\Downloads\py_dss_interface-master\src"
#                                                r"\py_dss_interface\dll/windows\x64", dll_by_user="OpenDSSDirect.dll")

# Select the DSS model
dss_file = r"C:\MeuTCC\Paulo_Example\DSSFiles\MASTER_RedeTeste13Barras.dss"
dss_file = r"C:\Program Files\OpenDSS\IEEETestCases\13Bus\IEEE13Nodeckt.dss"
# Compile
dss.text("compile [{}]".format(dss_file))
overload_file_path = pathlib.Path(dss_file).parent.joinpath(f"{dss.circuit_name()}_EXP_OVERLOADS.CSV")

# Solve
# dss.text('export overloads')
dss.text("? Load.611.kw")
dss.solution_solve()

dss.loadshapes_first()
dss.loadshapes_read_p_mult()

new = list(dss.loadshapes_read_p_mult())
new[2] = 0

dss.loadshapes_write_p_mult(new)
# Show Voltage Report

print("worked here")
print(dss.loadshapes_read_p_mult())
dss.text("show voltages")

print(dss.dss_read_datapath())
# Get all buses voltages

allbusvolts = dss.circuit_all_bus_volts()

print(dss.circuit_all_bus_volts())

