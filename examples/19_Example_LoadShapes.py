# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.loadshapes_count(): {dss.count()}')
print(f'dss.loadshapes_first(): {dss.first()}')
print(f'dss.loadshapes_next(): {dss.next()}')
print(f'dss.loadshapes_read_npts(): {dss.npts_read()}')
print(f'dss.loadshapes_write_npts(): {dss.npts_write(1)}')
print(f'dss.loadshapes_read_npts(): {dss.npts_read()}')
print(f'dss.loadshapes_normalize(): {dss.normalize()}')
print(f'dss.loadshapes_read_use_actual(): {dss.use_actual_read()}')
print(f'dss.loadshapes_write_use_actual(): {dss.use_actual_write(1)}')
print(f'dss.loadshapes_read_use_actual(): {dss.use_actual_read()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.loadshapes_read_name(): {dss.name_read()}')
# print(f'dss.loadshapes_write_name(): {dss.loadshapes_write_name("MyLoadShape")}')
print(f'dss.loadshapes_read_name(): {dss.name_read()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.loadshapes_read_hr_interval(): {dss.hr_interval_read()}')
print(f'dss.loadshapes_write_hr_interval(): {dss.hr_interval_write(1.5)}')
print(f'dss.loadshapes_read_hr_interval(): {dss.hr_interval_read()}')

print(f'dss.loadshapes_read_min_interval(): {dss.min_interval_read()}')
print(f'dss.loadshapes_write_min_interval(): {dss.min_interval_read(30.0)}')
print(f'dss.loadshapes_read_min_interval(): {dss.min_interval_read()}')

print(f'dss.loadshapes_read_p_base(): {dss.p_base_read()}')
print(f'dss.loadshapes_write_p_base(): {dss.p_base_write(0.2)}')
print(f'dss.loadshapes_read_p_base(): {dss.p_base_read()}')

print(f'dss.loadshapes_read_q_base(): {dss.q_base_read()}')
print(f'dss.loadshapes_write_q_base(): {dss.q_base_write(1.)}')
print(f'dss.loadshapes_read_q_base(): {dss.q_base_read()}')

print(f'dss.loadshapes_read_s_interval(): {dss.s_interval_read()}')
print(f'dss.loadshapes_write_s_interval(): {dss.s_interval_write(2.)}')
print(f'dss.loadshapes_read_s_interval(): {dss.s_interval_read()}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.loadshapes_all_names(): {dss.names()}')
print(f'dss.loadshapes_read_p_mult(): {dss.p_mult_read()}')
print(f'dss.loadshapes_write_p_mult(): {dss.p_mult_write("[1.23 2.0]")}')
print(f'dss.loadshapes_read_p_mult(): {dss.p_mult_read()}')

print(f'dss.loadshapes_read_q_mult(): {dss.q_mult_read()}')
print(f'dss.loadshapes_write_q_mult(): {dss.q_mult_write("[1.82 3.2]")}')
print(f'dss.loadshapes_read_q_mult(): {dss.q_mult_read()}')

print(f'dss.loadshapes_read_time_array(): {dss.time_array_read()}')
print(f'dss.loadshapes_write_time_array(): {dss.time_array_write("[1.25 1.21]")}')
print(f'dss.loadshapes_read_time_array(): {dss.time_array_read()}')
