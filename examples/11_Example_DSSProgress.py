# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.dssprogress_pct_progress(): {dss.dssprogress_pct_progress(12.5)}')
print(f'dss.dssprogress_show(): {dss.dssprogress_show()}')
print(f'dss.dssprogress_close(): {dss.dssprogress_close()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.dssprogress_caption(): {dss.dssprogress_caption("My Caption")}')
