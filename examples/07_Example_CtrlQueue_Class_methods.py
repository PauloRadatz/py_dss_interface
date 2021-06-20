# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 12/05/2021
"""
import os
import pathlib
from py_dss_interface import DSSDLL as DSS

dss = DSS()

my_path = pathlib.Path(__file__).parents[1] # one level above
my_path = os.path.join(my_path, r"test\py_dss_interface\13Bus")
dss_file = os.path.join(my_path, "IEEE13Nodeckt.dss")

dss.text("compile {0}".format(dss_file))

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.ctrlqueue_clearqueue(): {dss.ctrlqueue_clear_queue()}')
print(f'dss.ctrlqueue_delete(): {dss.ctrlqueue_delete()}')
print(f'dss.ctrlqueue_numactions(): {dss.ctrlqueue_num_actions()}')
print(f'dss.ctrlqueue_action(): {dss.ctrlqueue_action()}')
print(f'dss.ctrlqueue_actioncode(): {dss.ctrlqueue_action_code()}')
print(f'dss.ctrlqueue_devicehandle(): {dss.ctrlqueue_device_handle()}')
print(f'dss.ctrlqueue_push(): {dss.ctrlqueue_push()}')
print(f'dss.ctrlqueue_show(): {dss.ctrlqueue_show(dss)}')
print(f'dss.ctrlqueue_clearactions(): {dss.ctrlqueue_clear_actions()}')
print(f'dss.ctrlqueue_popaction(): {dss.ctrlqueue_pop_action()}')
print(f'dss.ctrlqueue_queuesize(): {dss.ctrlqueue_queue_size()}')
print(f'dss.ctrlqueue_doallqueue(): {dss.ctrlqueue_do_all_queue()}')

# To iterate from begin we must call first()
# dss.active_class_first()
# for i in range(dss.active_class_num_elements()):
#     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
#     dss.active_class_next()


# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')


# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')


# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
