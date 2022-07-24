# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# CtrlQueue
print(f'dss.ctrlqueue_ctrlqueue(): {dss.ctrlqueue()}')

# DSSElement
print(f'dss.dsselement_allpropertynames(): {dss.property_names()}')
print(f'dss.dss_classes(): {dss.classes()}')
print(f'dss.dss_user_classes(): {dss.user_classes()}')

# Fuses
print(f'dss.fuses_allnames(): {dss.names()}')

# Generators
print(f'dss.generators_allnames(): {dss.names()}')
print(f'dss.generators_registernames(): {dss.register_names()}')
print(f'dss.generators_registervalues(): {dss.register_values()}')

# ISources
print(f'dss.isources_allnames(): {dss.isources_all_names()}')

# LineCodes
print(f'dss.linecodes_read_rmatrix(): {dss.linecodes_read_rmatrix()}')
rmatrix = "[0.791721 | 0.318476 0.781649 | 0.28345, 0.318476, 0.791721]"
print(f'dss.linecodes_write_rmatrix(): {dss.linecodes_write_rmatrix(rmatrix)}')
print(f'dss.linecodes_read_rmatrix(): {dss.linecodes_read_rmatrix()}')

print(f'dss.linecodes_read_xmatrix(): {dss.linecodes_read_xmatrix()}')
xmatrix = "[0.438352  |0.0276838  0.396697  |-0.0184204  0.0276838  0.438352  ]"
print(f'dss.linecodes_write_xmatrix(): {dss.linecodes_write_xmatrix(xmatrix)}')
print(f'dss.linecodes_read_xmatrix(): {dss.linecodes_read_xmatrix()}')

print(f'dss.linecodes_read_cmatrix(): {dss.linecodes_read_cmatrix()}')
cmatrix = "[257 | 0 257 | 0 0 257]"
print(f'dss.linecodes_write_cmatrix(): {dss.linecodes_write_cmatrix(cmatrix)}')
print(f'dss.linecodes_read_cmatrix(): {dss.linecodes_read_cmatrix()}')

print(f'dss.linecodes_allnames(): {dss.linecodes_all_names()}')

# Lines
print(f'dss.lines_allnames(): {dss.lines_all_names()}')

print(f'dss.lines_read_rmatrix(): {dss.lines_read_rmatrix()}')
print(f'dss.lines_write_rmatrix(): {dss.lines_write_rmatrix(rmatrix)}')
print(f'dss.lines_read_rmatrix(): {dss.lines_read_rmatrix()}')

print(f'dss.lines_read_rmatrix(): {dss.lines_read_cmatrix()}')
print(f'dss.lines_write_cmatrix(): {dss.lines_write_cmatrix(cmatrix)}')
print(f'dss.lines_read_rmatrix(): {dss.lines_read_cmatrix()}')

print(f'dss.lines_read_yprim(): {dss.lines_read_yprim()}')

# Loads
print()
print(f'dss.loads_allnames(): {dss.loads_all_names()}')
print(f'dss.loads_read_zipv(): {dss.loads_read_zipv()}')
zipv = "[0 1 2 3 4 5 6]"
print(f'dss.loads_write_zipv(): {dss.loads_write_zipv(zipv)}')
print(f'dss.loads_read_zipv(): {dss.loads_read_zipv()}')

# LoadShapes
# print()
# print(f'dss.loadshapes_allnames(): {dss.loadshapes_allnames()}')
# print(f'dss.loadshapes_read_pmult(): {dss.loadshapes_read_pmult()}')
# pmult = "[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23]"
# print(f'dss.loadshapes_write_pmult(): {dss.loadshapes_write_pmult(pmult)}')
# print(f'dss.loadshapes_read_pmult(): {dss.loadshapes_read_pmult()}')
# print(f'dss.loadshapes_read_qmult(): {dss.loadshapes_read_qmult()}')
# qmult = "[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23]"
# print(f'dss.loadshapes_write_qmult(): {dss.loadshapes_write_qmult(qmult)}')
# print(f'dss.loadshapes_read_qmult(): {dss.loadshapes_read_qmult()}')
# npoints = "[0 2 4 6 8]"
# print(f'dss.loadshapes_write_timearray(): {dss.loadshapes_write_timearray(npoints)}')

# Meters
print()
print(f'dss.meters_allnames(): {dss.meters_all_names()}')
print(f'dss.meters_registernames(): {dss.meters_register_names()}')
print(f'dss.meters_registervalues(): {dss.meters_register_values()}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peak_current()}')
print(f'dss.meters_totals(): {dss.meters_totals()}')
peak_current = "[400 400 400 ]"
print(f'dss.meters_write_peakcurrent(): {dss.meters_write_peak_current(peak_current)}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peak_current()}')
print(f'dss.meters_read_calcurrent(): {dss.meters_read_cal_current()}')
# print(f'dss.meters_write_calcurrent(): {dss.meters_write_calcurrent(12.5)}')
print(f'dss.meters_read_calcurrent(): {dss.meters_read_cal_current()}')
print(f'dss.meters_read_allocfactors(): {dss.meters_read_alloc_factors()}')

# Sensors
print(f'dss.sensors_allnames(): {dss.sensors_all_names()}')
print(f'dss.sensors_read_currents(): {dss.sensors_read_currents()}')
currents = "[12 58 96 36]"
print(f'dss.sensors_write_currents(): {dss.sensors_write_currents(currents)}')
print(f'dss.sensors_read_currents(): {dss.sensors_read_currents()}')
print(f'dss.sensors_read_kvars(): {dss.sensors_read_kvars()}')
kvars = "[25 69 138]"
print(f'dss.sensors_write_kvars(): {dss.sensors_write_kvars(kvars)}')
print(f'dss.sensors_read_kws(): {dss.sensors_read_kws()}')
kws = "[25 36 96]"
print(f'dss.sensors_write_kws(): {dss.sensors_write_kws(kws)}')
print(f'dss.sensors_read_kws(): {dss.sensors_read_kws()}')

# XYCurves
print(f'dss.read_xarray(): {dss.read_x_array()}')
