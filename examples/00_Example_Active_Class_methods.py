# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 12/05/2021
"""
from py_dss_interface import DSS

# import pathlib

dss = DSS()

dss_file = r"C:\eniocc\EPRI\py_dss_interface-master\src\py_dss_interface\tests\py_dss_interface\13Bus\IEEE13Nodeckt" \
           r".dss "

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
