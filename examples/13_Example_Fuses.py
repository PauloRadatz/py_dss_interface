# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.fuses_count(): {dss._count()}')
print(f'dss.fuses_first(): {dss.first()}')
print(f'dss.fuses_next(): {dss.next()}')
print(f'dss.fuses_read_monitored_term(): {dss.monitored_term_read()}')
print(f'dss.fuses_write_monitored_term(): {dss.monitored_term_write(1)}')
print(f'dss.fuses_read_switched_term(): {dss.switched_term_read()}')
print(f'dss.fuses_write_switched_term(): {dss.switched_term_write(2)}')
print(f'dss.fuses_open(): {dss.open()}')
print(f'dss.fuses_close(): {dss.close()}')
print(f'dss.fuses_is_blown(): {dss.is_blown()}')
print(f'dss.fuses_read_idx(): {dss.idx_read()}')
print(f'dss.fuses_write_idx(): {dss.idx_write(1)}')
print(f'dss.fuses_num_phases(): {dss.num_phases()}')


# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.fuses_read_name(): {dss.name_read()}')
print(f'dss.fuses_write_name(): {dss.name_write("")}')
print(f'dss.fuses_read_monitored_obj(): {dss.monitored_obj_read()}')
print(f'dss.fuses_write_monitored_obj(): {dss.monitored_obj_read("")}')
print(f'dss.fuses_read_switched_obj(): {dss.switched_obj_write()}')
print(f'dss.fuses_write_switched_obj(): {dss.switched_obj_write("")}')
print(f'dss.fuses_read_tcc_curve(): {dss.tcc_curve_read()}')
print(f'dss.fuses_write_tcc_curve(): {dss.tcc_curve_write("[1 1 2 3 4]")}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.fuses_read_rated_current(): {dss.rated_current_read()}')
print(f'dss.fuses_write_rated_current(): {dss.rated_current_write("12.5")}')
print(f'dss.fuses_read_rated_current(): {dss.rated_current_read()}')
print(f'dss.fuses_read_delay(): {dss.delay_read()}')
print(f'dss.fuses_write_delay(): {dss.delay_write("23.68")}')
print(f'dss.fuses_read_delay(): {dss.delay_read()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.fuses_all_names(): {dss.names()}')

