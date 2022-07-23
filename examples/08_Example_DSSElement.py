# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dsselement_num_properties: {dss.num_properties()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dsselement_name: {dss.name()}')


# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dsselement_all_property_names: {dss.property_names()}')
