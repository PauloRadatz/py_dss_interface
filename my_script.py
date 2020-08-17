# First import the Package
import py_dss_interface

# Creates an OpenDSS object
dss = py_dss_interface.DSSDLL()

# Select the DSS model
dss_file = r"C:\MeuTCC\Paulo_Example\DSSFiles\MASTER_RedeTeste13Barras.dss"

# Compile
dss.text("compile {}".format(dss_file))

# Solve
dss.solution_solve()

# Show Voltage Report
dss.text("show voltages")

# Get all buses voltages
allbusvolts = dss.circuit_allbusvolts()

print(dss.circuit_allbusvolts())

