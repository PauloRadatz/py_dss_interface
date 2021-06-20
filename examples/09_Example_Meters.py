# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 15/05/2021
"""
import os
import pathlib
from py_dss_interface import DSSDLL as DSS

dss = DSS()

my_path = pathlib.Path(__file__).parents[1] # one level above
my_path = os.path.join(my_path, r"test\py_dss_interface\13Bus")
dss_file = os.path.join(my_path, "IEEE13Nodeckt.dss")

dss.text("compile {0}".format(dss_file))

print(f'dss.meters_allnames(): {dss.meters_all_names()}')
print(f'dss.meters_registernames(): {dss.meters_register_names()}')
print(f'dss.meters_registervalues(): {dss.meters_register_values()}')
print(f'dss.meters_totals(): {dss.meters_totals()}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peak_current()}')
print(f'dss.meters_write_peakcurrent(): {dss.meters_write_peak_current("[550,600,680]")}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peak_current()}')
print(f'dss.meters_read_calcurrent(): {dss.meters_read_cal_current()}')

print(f'dss.meters_read_allocfactors(): {dss.meters_read_alloc_factors()}')
print(f'dss.meters_write_allocfactors(): {dss.meters_write_alloc_factors("[0.1]")}')

print(f'dss.meters_allendelements(): {dss.meters_all_end_elements()}')
print(f'dss.meters_allbranchesinzone(): {dss.meters_all_branches_in_zone()}')

print(f'dss.monitors_channel(): {dss.monitors_channel("2")}')
print(f'dss.monitors_allnames(): {dss.monitors_all_names()}')

