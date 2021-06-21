# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.loadshapes_count(): {dss.loadshapes_count()}')
print(f'dss.loadshapes_first(): {dss.loadshapes_first()}')
print(f'dss.loadshapes_next(): {dss.loadshapes_next()}')
print(f'dss.loadshapes_read_npts(): {dss.loadshapes_read_npts()}')
print(f'dss.loadshapes_write_npts(): {dss.loadshapes_write_npts(1)}')
print(f'dss.loadshapes_read_npts(): {dss.loadshapes_read_npts()}')
print(f'dss.loadshapes_normalize(): {dss.loadshapes_normalize()}')
print(f'dss.loadshapes_read_use_actual(): {dss.loadshapes_read_use_actual()}')
print(f'dss.loadshapes_write_use_actual(): {dss.loadshapes_write_use_actual(1)}')
print(f'dss.loadshapes_read_use_actual(): {dss.loadshapes_read_use_actual()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.loadshapes_read_name(): {dss.loadshapes_read_name()}')
# print(f'dss.loadshapes_write_name(): {dss.loadshapes_write_name("MyLoadShape")}')
print(f'dss.loadshapes_read_name(): {dss.loadshapes_read_name()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.loadshapes_read_hr_interval(): {dss.loadshapes_read_hr_interval()}')
print(f'dss.loadshapes_write_hr_interval(): {dss.loadshapes_write_hr_interval(1.5)}')
print(f'dss.loadshapes_read_hr_interval(): {dss.loadshapes_read_hr_interval()}')

print(f'dss.loadshapes_read_min_interval(): {dss.loadshapes_read_min_interval()}')
print(f'dss.loadshapes_write_min_interval(): {dss.loadshapes_write_min_interval(30.0)}')
print(f'dss.loadshapes_read_min_interval(): {dss.loadshapes_read_min_interval()}')

print(f'dss.loadshapes_read_p_base(): {dss.loadshapes_read_p_base()}')
print(f'dss.loadshapes_write_p_base(): {dss.loadshapes_write_p_base(0.2)}')
print(f'dss.loadshapes_read_p_base(): {dss.loadshapes_read_p_base()}')

print(f'dss.loadshapes_read_q_base(): {dss.loadshapes_read_q_base()}')
print(f'dss.loadshapes_write_q_base(): {dss.loadshapes_write_q_base(1.)}')
print(f'dss.loadshapes_read_q_base(): {dss.loadshapes_read_q_base()}')

print(f'dss.loadshapes_read_s_interval(): {dss.loadshapes_read_s_interval()}')
print(f'dss.loadshapes_write_s_interval(): {dss.loadshapes_write_s_interval(2.)}')
print(f'dss.loadshapes_read_s_interval(): {dss.loadshapes_read_s_interval()}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.loadshapes_all_names(): {dss.loadshapes_all_names()}')
print(f'dss.loadshapes_read_p_mult(): {dss.loadshapes_read_p_mult()}')
print(f'dss.loadshapes_write_p_mult(): {dss.loadshapes_write_p_mult("[1.23 2.0]")}')
print(f'dss.loadshapes_read_p_mult(): {dss.loadshapes_read_p_mult()}')

print(f'dss.loadshapes_read_q_mult(): {dss.loadshapes_read_q_mult()}')
print(f'dss.loadshapes_write_q_mult(): {dss.loadshapes_write_q_mult("[1.82 3.2]")}')
print(f'dss.loadshapes_read_q_mult(): {dss.loadshapes_read_q_mult()}')

print(f'dss.loadshapes_read_time_array(): {dss.loadshapes_read_time_array()}')
print(f'dss.loadshapes_write_time_array(): {dss.loadshapes_write_time_array("[1.25 1.21]")}')
print(f'dss.loadshapes_read_time_array(): {dss.loadshapes_read_time_array()}')
