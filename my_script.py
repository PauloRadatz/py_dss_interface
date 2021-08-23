# First import the Package
import py_dss_interface
import pathlib
import os
# Creates an OpenDSS object
dss = py_dss_interface.DSSDLL("C:\OpenDSS_svn\Version8\Source")
# dss = py_dss_interface.DSSDLL(r"C:\Program Files\OpenDSS")

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

dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")

dss.loadshapes_write_name('test')

dss.loadshapes_write_hr_interval(1000)

dss.text("New TCC_Curve.tlink  npts=7 C_array=[ 2 2.1 3 4 6 22 50] T_array=[ 300 100 10.1 4 1.4 0.1 0.02]")
dss.text("New TCC_Curve.tlink2 npts=7 C_array=[ 2 2.1 3 4 6 22 50] T_array=[ 300 100 10.1 4 1.4 0.1 0.02]")
dss.text("New Fuse.Fuse1   LINE.684652   1 fusecurve=tlink   Ratedcurrent=10")
dss.text("New Fuse.Fuse2   LINE.684611   1 fusecurve=tlink2  Ratedcurrent=15")
dss.fuses_write_name("Fuse1")



dss.lines_write_name('650632')

dss.text("New Generator.G1  Bus1=645.2 phases=1  kV=2.4 kW=100 kvar=-50 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400")
dss.generators_write_name("G1")

a = dss.generators_read_kw()
b = dss.generators_write_kw(200)
c = dss.generators_read_kw()

dss.linecodes_write_name('1')
expected = 1.0
dss.linecodes_write_c1(expected)
dss.linecodes_read_c1()

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

