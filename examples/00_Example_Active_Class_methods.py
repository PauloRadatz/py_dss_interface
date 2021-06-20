# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 12/05/2021
"""
import os
import pathlib
from py_dss_interface import DSSDLL as DSS

dss = DSS()

my_path = pathlib.Path(__file__).parents[1] # one level above
my_path = os.path.join(my_path, r"tests\py_dss_interface\13Bus")
dss_file = os.path.join(my_path, "IEEE13Nodeckt.dss")

dss.text("compile {0}".format(dss_file))

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.active_class_get_class_name(): {dss.active_class_get_class_name()}')
print(f'dss.active_class_get_name(): {dss.active_class_get_name()}')
print(f'dss.active_class_first(): {dss.active_class_first()}')
print(f'dss.active_class_num_elements(): {dss.active_class_num_elements()}')
print(f'dss.active_class_count(): {dss.active_class_count()}')
print()
print(f'dss.active_class_get_class_name(): {dss.active_class_get_class_name()}')
print(f'dss.active_class_get_name(): {dss.active_class_get_name()}')
print(f'dss.active_class_next(): {dss.active_class_next()}')
print(f'dss.active_class_num_elements(): {dss.active_class_num_elements()}')
print(f'dss.active_class_count(): {dss.active_class_count()}')

# To iterate from begin we must call first()
dss.active_class_first()
for i in range(dss.active_class_num_elements()):
    print(f'Name: {dss.active_class_get_name()} || Index: {i}')
    dss.active_class_next()


# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
dss.active_class_first()
print(f'dss.active_class_get_name(): {dss.active_class_get_name()}')
print(f'dss.active_class_get_class_name(): {dss.active_class_get_class_name()} \n')
print(f'dss.active_class_parent_class_name(): {dss.active_class_parent_class_name()} \n')

dss.active_class_first()
for _ in range(dss.active_class_num_elements()):
    print(f'dss.active_class_get_name(): {dss.active_class_get_name()}')
    dss.active_class_next()

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(dss.active_class_all_names())
