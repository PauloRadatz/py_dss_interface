# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods ' + 45 * '=')
dss.first()
print(f'dss.capacitors_read_num_step(): {dss.read_num_steps()}')
print(f'dss.capacitors_write_num_steps(): {dss.write_num_steps(5)}')
print(f'dss.capacitors_read_num_step(): {dss.read_num_steps()}')
print(f'dss.capacitors_available_steps(): {dss.available_steps()}')
print()
print(f'dss.capacitors_read_isdelta(): {dss.is_delta()}')
print(f'dss.capacitors_write_isdelta(): {dss.write_is_delta()}')
print(f'dss.capacitors_read_isdelta(): {dss.is_delta()}')
print()
print(f'dss.capacitors_count(): {dss.count()}')
print(f'dss.capacitors_first(): {dss.first()}')
print(f'dss.capacitors_read_num_steps(): {dss.read_num_steps()}')
print(f'dss.capacitors_available_steps(): {dss.available_steps()}')
print(f'dss.capacitors_add_step(): {dss.add_step()}')
print(f'dss.capacitors_read_num_steps(): {dss.read_num_steps()}')
print(f'dss.capacitors_available_steps(): {dss.available_steps()}')
print(f'dss.capacitors_subtract_step(): {dss.subtract_step()}')
print(f'dss.capacitors_read_num_teps(): {dss.read_num_steps()}')
print(f'dss.capacitors_subtract_step(): {dss.subtract_step()}')
print(f'dss.capacitors_available_steps(): {dss.available_steps()}')
print(f'dss.capacitors_read_states(): {dss.read_states()}')
print(f'dss.capacitors_open(): {dss.open_all_steps()}')
print(f'dss.capacitors_read_states(): {dss.read_states()}')
print(f'dss.capacitors_read_num_steps(): {dss.read_num_steps()}')
print(f'dss.capacitors_available_steps(): {dss.available_steps()}')
print(f'dss.capacitors_close(): {dss.close_all_steps()}')
print(f'dss.capacitors_read_states(): {dss.read_states()}')
print(f'dss.capacitors_read_num_steps(): {dss.read_num_steps()}')
print(f'dss.capacitors_available_steps(): {dss.available_steps()}')

# To iterate from begin we must call first()
dss.first()
for i in range(dss.count()):
    print(f'Name: {dss.read_name()} || Index: {i}')
    dss.next()

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.capacitors_read_kv(): {dss.read_kv()}')
print(f'dss.capacitors_write_kv(): {dss.write_kv(2.6)}')
print(f'dss.capacitors_read_kv(): {dss.read_kv()}')

print(f'dss.capacitors_read_kvar(): {dss.read_kvar()}')
print(f'dss.capacitors_write_kvar(): {dss.write_kvar(102.6)}')
print(f'dss.capacitors_read_kvar(): {dss.read_kvar()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.capacitors_read_name(): {dss.read_name()}')
print(f'dss.capacitors_write_name(): {dss.write_name("cap2")}')
print(f'dss.capacitors_read_name(): {dss.read_name()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.capacitors_allnames(): {dss.all_names()}')
dss.first()
print(f'dss.capacitors_read_states(): {dss.read_states()}')
print(f'dss.capacitors_write_states(): {dss.write_states(dss, [0, 1, 0, 1, 1])}')
print(f'dss.capacitors_read_states(): {dss.read_states()}')
