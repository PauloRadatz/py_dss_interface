# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.dss_num_circuits(): {dss.dss_num_circuits()}')
print(f'dss.dss_clear_all(): {dss.dss_clear_all()}')
print(f'dss.dss_show_panel(): {dss.dss_show_panel()}')
print(f'dss.dss_start(): {dss.dss_start()}')
print(f'dss.dss_num_classes(): {dss.dss_num_classes()}')
print(f'dss.dss_num_user_classes(): {dss.dss_num_user_classes()}')
print(f'dss.dss_reset(): {dss.dss_reset()}')
print(f'dss.dss_read_allow_forms(): {dss.dss_read_allow_forms()}')
print(f'dss.dss_write_allow_forms(): {dss.dss_write_allow_forms(0)}')
print(f'dss.dss_read_allow_forms(): {dss.dss_read_allow_forms()}')

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.dss_new_circuit(): {dss.dss_new_circuit("new_rest_circuit")}')
print(f'dss.dss_version(): {dss.dss_version()}')
# print(f'dss.dss_read_datapath(): {dss.dss_read_datapath()}')
# dss.dss_write_datapath("D:")
# print(f'dss.dss_read_datapath(): {dss.dss_read_datapath()}')
print(f'dss.dss_default_editor(): {dss.dss_default_editor()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.dss_classes(): {dss.dss_classes()}')
print(f'dss.dss_user_classes(): {dss.dss_user_classes()}')
