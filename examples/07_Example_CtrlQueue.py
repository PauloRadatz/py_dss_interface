# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.ctrlqueue_ctrlqueue(): {dss.ctrlqueue_ctrlqueue()}')

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.ctrlqueue_clear_queue(): {dss.ctrlqueue_clear_queue()}')
print(f'dss.ctrlqueue_delete(): {dss.ctrlqueue_delete()}')
print(f'dss.ctrlqueue_num_actions(): {dss.ctrlqueue_num_actions()}')
print(f'dss.ctrlqueue_action(): {dss.ctrlqueue_action()}')
print(f'dss.ctrlqueue_action_code(): {dss.ctrlqueue_action_code()}')
print(f'dss.ctrlqueue_device_handle(): {dss.ctrlqueue_device_handle()}')
print(f'dss.ctrlqueue_push(): {dss.ctrlqueue_push()}')
print(f'dss.ctrlqueue_show(): {dss.ctrlqueue_show()}')
print(f'dss.ctrlqueue_clear_actions(): {dss.ctrlqueue_clear_actions()}')
print(f'dss.ctrlqueue_pop_action(): {dss.ctrlqueue_pop_action()}')
print(f'dss.ctrlqueue_queue_size(): {dss.ctrlqueue_queue_size()}')
print(f'dss.ctrlqueue_do_all_queue(): {dss.ctrlqueue_do_all_queue()}')



