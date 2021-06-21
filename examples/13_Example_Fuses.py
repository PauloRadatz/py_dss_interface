# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.fuses_count(): {dss.fuses_count()}')
print(f'dss.fuses_first(): {dss.fuses_first()}')
print(f'dss.fuses_next(): {dss.fuses_next()}')
print(f'dss.fuses_read_monitored_term(): {dss.fuses_read_monitored_term()}')
print(f'dss.fuses_write_monitored_term(): {dss.fuses_write_monitored_term(1)}')
print(f'dss.fuses_read_switched_term(): {dss.fuses_read_switched_term()}')
print(f'dss.fuses_write_switched_term(): {dss.fuses_write_switched_term(2)}')
print(f'dss.fuses_open(): {dss.fuses_open()}')
print(f'dss.fuses_close(): {dss.fuses_close()}')
print(f'dss.fuses_is_blown(): {dss.fuses_is_blown()}')
print(f'dss.fuses_read_idx(): {dss.fuses_read_idx()}')
print(f'dss.fuses_write_idx(): {dss.fuses_write_idx(1)}')
print(f'dss.fuses_num_phases(): {dss.fuses_num_phases()}')


# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.fuses_read_name(): {dss.fuses_read_name()}')
print(f'dss.fuses_write_name(): {dss.fuses_write_name("")}')
print(f'dss.fuses_read_monitored_obj(): {dss.fuses_read_monitored_obj()}')
print(f'dss.fuses_write_monitored_obj(): {dss.fuses_write_monitored_obj("")}')
print(f'dss.fuses_read_switched_obj(): {dss.fuses_read_switched_obj()}')
print(f'dss.fuses_write_switched_obj(): {dss.fuses_write_switched_obj("")}')
print(f'dss.fuses_read_tcc_curve(): {dss.fuses_read_tcc_curve()}')
print(f'dss.fuses_write_tcc_curve(): {dss.fuses_write_tcc_curve("[1 1 2 3 4]")}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.fuses_read_rated_current(): {dss.fuses_read_rated_current()}')
print(f'dss.fuses_write_rated_current(): {dss.fuses_write_rated_current("12.5")}')
print(f'dss.fuses_read_rated_current(): {dss.fuses_read_rated_current()}')
print(f'dss.fuses_read_delay(): {dss.fuses_read_delay()}')
print(f'dss.fuses_write_delay(): {dss.fuses_write_delay("23.68")}')
print(f'dss.fuses_read_delay(): {dss.fuses_read_delay()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.fuses_all_names(): {dss.fuses_all_names()}')

