# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 12/05/2021
"""
from py_dss_interface import DSS

# import pathlib

dss = DSS()

dss_file = r"C:\eniocc\EPRI\py_dss_interface-master\src\py_dss_interface\tests\py_dss_interface\13Bus\IEEE13Nodeckt" \
           r".dss "

dss.text("compile {0}".format(dss_file))

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.ctrlqueue_clearqueue(): {dss.ctrlqueue_clearqueue()}')
print(f'dss.ctrlqueue_delete(): {dss.ctrlqueue_delete()}')
print(f'dss.ctrlqueue_numactions(): {dss.ctrlqueue_numactions()}')
print(f'dss.ctrlqueue_action(): {dss.ctrlqueue_action()}')
print(f'dss.ctrlqueue_actioncode(): {dss.ctrlqueue_actioncode()}')
print(f'dss.ctrlqueue_devicehandle(): {dss.ctrlqueue_devicehandle()}')
print(f'dss.ctrlqueue_push(): {dss.ctrlqueue_push()}')
print(f'dss.ctrlqueue_show(): {dss.ctrlqueue_show(dss)}')
print(f'dss.ctrlqueue_clearactions(): {dss.ctrlqueue_clearactions()}')
print(f'dss.ctrlqueue_popaction(): {dss.ctrlqueue_popaction()}')
print(f'dss.ctrlqueue_queuesize(): {dss.ctrlqueue_queuesize()}')
print(f'dss.ctrlqueue_doallqueue(): {dss.ctrlqueue_doallqueue()}')

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
