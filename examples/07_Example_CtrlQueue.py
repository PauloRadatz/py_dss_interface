# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.ctrlqueue_ctrlqueue(): {dss.ctrlqueue()}')

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.ctrlqueue_clear_queue(): {dss.clear_queue()}')
print(f'dss.ctrlqueue_delete(): {dss.delete()}')
print(f'dss.ctrlqueue_num_actions(): {dss.num_actions()}')
print(f'dss.ctrlqueue_action(): {dss.action()}')
print(f'dss.ctrlqueue_action_code(): {dss.action_code()}')
print(f'dss.ctrlqueue_device_handle(): {dss.device_handle()}')
print(f'dss.ctrlqueue_push(): {dss.push()}')
print(f'dss.ctrlqueue_show(): {dss.show()}')
print(f'dss.ctrlqueue_clear_actions(): {dss.clear_actions()}')
print(f'dss.ctrlqueue_pop_action(): {dss.pop_action()}')
print(f'dss.ctrlqueue_queue_size(): {dss.queue_size()}')
print(f'dss.ctrlqueue_do_all_queue(): {dss.do_all_queue()}')



