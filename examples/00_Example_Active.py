# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.active_class_get_class_name(): {dss.get_class_name()}')
print(f'dss.active_class_get_name(): {dss.get_name()}')
print(f'dss.active_class_first(): {dss.first()}')
print(f'dss.active_class_num_elements(): {dss.num_elements()}')
print(f'dss.active_class_count(): {dss.count()}')
print()
print(f'dss.active_class_get_class_name(): {dss.get_class_name()}')
print(f'dss.active_class_get_name(): {dss.get_name()}')
print(f'dss.active_class_next(): {dss.next()}')
print(f'dss.active_class_num_elements(): {dss.num_elements()}')
print(f'dss.active_class_count(): {dss.count()}')

# To iterate from begin we must call first()
dss.first()
for i in range(dss.num_elements()):
    print(f'Name: {dss.get_name()} || Index: {i}')
    dss.next()


# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
dss.first()
print(f'dss.active_class_get_name(): {dss.get_name()}')
print(f'dss.active_class_write_name(): {dss.write_name("645646")}')
print(f'dss.active_class_get_name(): {dss.get_name()}')
print(f'dss.active_class_get_class_name(): {dss.get_class_name()} \n')
print(f'dss.active_class_parent_class_name(): {dss.parent_class_name()} \n')

dss.first()
for _ in range(dss.num_elements()):
    print(f'dss.active_class_get_name(): {dss.get_name()}')
    dss.next()

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(dss.all_names())
