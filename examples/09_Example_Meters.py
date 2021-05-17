# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 15/05/2021
"""
from py_dss_interface import DSS

dss = DSS()

dss_file = r"C:\eniocc\EPRI\py_dss_interface-master\src\py_dss_interface\tests\py_dss_interface\13Bus\IEEE13Nodeckt" \
           r".dss "

dss.text("compile {0}".format(dss_file))
print(f'dss.meters_allnames(): {dss.meters_allnames()}')
print(f'dss.meters_registernames(): {dss.meters_registernames()}')
print(f'dss.meters_registervalues(): {dss.meters_registervalues()}')
print(f'dss.meters_totals(): {dss.meters_totals()}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peakcurrent()}')
print(f'dss.meters_write_peakcurrent(): {dss.meters_write_peakcurrent("[550,600,680]")}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peakcurrent()}')
print(f'dss.meters_read_calcurrent(): {dss.meters_read_calcurrent()}')

print(f'dss.meters_read_allocfactors(): {dss.meters_read_allocfactors()}')
print(f'dss.meters_write_allocfactors(): {dss.meters_write_allocfactors("[0.1]")}')

print(f'dss.meters_allendelements(): {dss.meters_allendelements()}')
print(f'dss.meters_allbranchesinzone(): {dss.meters_allbranchesinzone()}')

print(f'dss.monitors_channel(): {dss.monitors_channel("2")}')
print(f'dss.monitors_allnames(): {dss.monitors_allnames()}')

