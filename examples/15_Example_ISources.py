# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss
dss.text("new isource.MyISource phases=3 amps=100 bus1=692 angle=30")

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.isources_count(): {dss.count()}')
print(f'dss.isources_first(): {dss.first()}')
print(f'dss.isources_next(): {dss.next()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.isources_read_name(): {dss.name_read()}')
print(f'dss.isources_write_name(): {dss.name_write("MyISource")}')
print(f'dss.isources_read_name(): {dss.name_read()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.isources_read_amps(): {dss.amps_read()}')
print(f'dss.isources_write_amps(): {dss.amps_write(12)}')
print(f'dss.isources_read_amps(): {dss.amps_read()}')
print(f'dss.isources_read_angle_deg(): {dss.angle_deg_read()}')
print(f'dss.isources_write_angle_deg(): {dss.angle_deg_write(5)}')
print(f'dss.isources_read_angle_deg(): {dss.angle_deg_read()}')
print(f'dss.isources_read_frequency(): {dss.frequency_read()}')
print(f'dss.isources_write_frequency(): {dss.frequency_write(22)}')
print(f'dss.isources_read_frequency(): {dss.frequency_read()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.isources_all_names(): {dss.names()}')
