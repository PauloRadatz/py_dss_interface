# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.monitors_first(): {dss.monitors_first()}')
print(f'dss.monitors_next(): {dss.monitors_next()}')
print(f'dss.monitors_reset(): {dss.monitors_reset()}')
print(f'dss.monitors_reset_all(): {dss.monitors_reset_all()}')
print(f'dss.monitors_sample(): {dss.monitors_sample()}')
print(f'dss.monitors_save(): {dss.monitors_save()}')
print(f'dss.monitors_show(): {dss.monitors_show()}')
print(f'dss.monitors_read_mode(): {dss.monitors_read_mode()}')
print(f'dss.monitors_write_mode(): {dss.monitors_write_mode(1)}')
print(f'dss.monitors_read_mode(): {dss.monitors_read_mode()}')

if dss.monitors_first() != 0:
    print(f'dss.monitors_sample_count(): {dss.monitors_sample_count()}')

print(f'dss.monitors_sample_all(): {dss.monitors_sample_all()}')
print(f'dss.monitors_save_all(): {dss.monitors_save_all()}')
print(f'dss.monitors_count(): {dss.monitors_count()}')
print(f'dss.monitors_process(): {dss.monitors_process()}')
print(f'dss.monitors_process_all(): {dss.monitors_process_all()}')
print(f'dss.monitors_file_version(): {dss.monitors_file_version()}')

if dss.monitors_first() != 0:
    print(f'dss.monitors_record_size(): {dss.monitors_record_size()}')
    print(f'dss.monitors_num_channels(): {dss.monitors_num_channels()}')
print(f'dss.monitors_read_terminal(): {dss.monitors_read_terminal()}')
print(f'dss.monitors_write_terminal(): {dss.monitors_write_terminal(1)}')


# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.monitors_file_name(): {dss.monitors_file_name()}')
print(f'dss.monitors_read_name(): {dss.monitors_read_name()}')
print(f'dss.monitors_write_name(): {dss.monitors_write_name("MyMonitor")}')
print(f'dss.monitors_read_element(): {dss.monitors_read_element()}')
print(f'dss.monitors_write_element(): {dss.monitors_write_element("MyElement")}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.monitors_all_names(): {dss.monitors_all_names()}')
if dss.monitors_first() != 0:
    print(f'dss.monitors_byte_stream(): {dss.monitors_byte_stream()}')
    print(f'dss.monitors_header(): {dss.monitors_header()}')
    print(f'dss.monitors_dbl_hour(): {dss.monitors_dbl_hour()}')
    print(f'dss.monitors_dbl_freq(): {dss.monitors_dbl_freq()}')
    print(f'dss.monitors_channel(): {dss.monitors_channel()}')

