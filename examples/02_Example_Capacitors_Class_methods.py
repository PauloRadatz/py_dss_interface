# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods ' + 45 * '=')
dss.capacitors_first()
print(f'dss.capacitors_read_nums_step(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_write_num_steps(): {dss.capacitors_write_num_steps(5)}')
print(f'dss.capacitors_read_nums_step(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_available_steps(): {dss.capacitors_available_steps()}')
print()
print(f'dss.capacitors_read_isdelta(): {dss.capacitors_read_is_delta()}')
print(f'dss.capacitors_write_isdelta(): {dss.capacitors_write_is_delta()}')
print(f'dss.capacitors_read_isdelta(): {dss.capacitors_read_is_delta()}')
print()
print(f'dss.capacitors_count(): {dss.capacitors_count()}')
print(f'dss.capacitors_first(): {dss.capacitors_first()}')
print(f'dss.capacitors_read_nums_steps(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_available_steps(): {dss.capacitors_available_steps()}')
print(f'dss.capacitors_add_step(): {dss.capacitors_add_step()}')
print(f'dss.capacitors_read_nums_steps(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_available_steps(): {dss.capacitors_available_steps()}')
print(f'dss.capacitors_subtract_step(): {dss.capacitors_subtract_step()}')
print(f'dss.capacitors_read_nums_teps(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_subtract_step(): {dss.capacitors_subtract_step()}')
print(f'dss.capacitors_available_steps(): {dss.capacitors_available_steps()}')
print(f'dss.capacitors_read_states(): {dss.capacitors_read_states()}')
print(f'dss.capacitors_open(): {dss.capacitors_open()}')
print(f'dss.capacitors_read_states(): {dss.capacitors_read_states()}')
print(f'dss.capacitors_read_nums_steps(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_available_steps(): {dss.capacitors_available_steps()}')
print(f'dss.capacitors_close(): {dss.capacitors_close()}')
print(f'dss.capacitors_read_states(): {dss.capacitors_read_states()}')
print(f'dss.capacitors_read_nums_steps(): {dss.capacitors_read_nums_steps()}')
print(f'dss.capacitors_available_steps(): {dss.capacitors_available_steps()}')

# To iterate from begin we must call first()
dss.capacitors_first()
for i in range(dss.capacitors_count()):
    print(f'Name: {dss.capacitors_read_name()} || Index: {i}')
    dss.capacitors_next()

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.capacitors_read_kv(): {dss.capacitors_read_kv()}')
print(f'dss.capacitors_write_kv(): {dss.capacitors_write_kv(2.6)}')
print(f'dss.capacitors_read_kv(): {dss.capacitors_read_kv()}')

print(f'dss.capacitors_read_kvar(): {dss.capacitors_read_kvar()}')
print(f'dss.capacitors_write_kvar(): {dss.capacitors_write_kvar(102.6)}')
print(f'dss.capacitors_read_kvar(): {dss.capacitors_read_kvar()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.capacitors_read_name(): {dss.capacitors_read_name()}')
print(f'dss.capacitors_write_name(): {dss.capacitors_write_name("cap2")}')
print(f'dss.capacitors_read_name(): {dss.capacitors_read_name()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.capacitors_allnames(): {dss.capacitors_all_names()}')
dss.capacitors_first()
print(f'dss.capacitors_read_states(): {dss.capacitors_read_states()}')
print(f'dss.capacitors_write_states(): {dss.capacitors_write_states(dss, [0, 1, 0, 1, 1])}')
print(f'dss.capacitors_read_states(): {dss.capacitors_read_states()}')
