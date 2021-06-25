# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss
dss.text("new isource.MyISource phases=3 amps=100 bus1=692 angle=30")

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.isources_count(): {dss.isources_count()}')
print(f'dss.isources_first(): {dss.isources_first()}')
print(f'dss.isources_next(): {dss.isources_next()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.isources_read_name(): {dss.isources_read_name()}')
print(f'dss.isources_write_name(): {dss.isources_write_name("MyISource")}')
print(f'dss.isources_read_name(): {dss.isources_read_name()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.isources_read_amps(): {dss.isources_read_amps()}')
print(f'dss.isources_write_amps(): {dss.isources_write_amps(12)}')
print(f'dss.isources_read_amps(): {dss.isources_read_amps()}')
print(f'dss.isources_read_angle_deg(): {dss.isources_read_angle_deg()}')
print(f'dss.isources_write_angle_deg(): {dss.isources_write_angle_deg(5)}')
print(f'dss.isources_read_angle_deg(): {dss.isources_read_angle_deg()}')
print(f'dss.isources_read_frequency(): {dss.isources_read_frequency()}')
print(f'dss.isources_write_frequency(): {dss.isources_write_frequency(22)}')
print(f'dss.isources_read_frequency(): {dss.isources_read_frequency()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.isources_all_names(): {dss.isources_all_names()}')
