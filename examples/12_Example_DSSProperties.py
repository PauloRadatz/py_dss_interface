# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# General methods
print(45 * '=' + ' General Methods' + 45 * '=')
print(f'dss.dssproperties_name(): {dss.name("1")}')
print(f'dss.dssproperties_description(): {dss.description("1")}')

print(f'dss.dssproperties_read_value(): {dss.value_read("1")}')

print(f'dss.dssproperties_write_value(): {dss.value_write("1")}')
print(f'dss.dssproperties_read_value(): {dss.value_read("1")}')


print(f'dss.dssproperties_write_value(): {dss.value_write("2")}')
print(f'dss.dssproperties_read_value(): {dss.value_read("1")}')
