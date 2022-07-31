# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.dss_num_circuits(): {dss.num_circuits()}')
print(f'dss.dss_clear_all(): {dss.clear_all()}')
print(f'dss.dss_show_panel(): {dss.show_panel()}')
print(f'dss.dss_start(): {dss.start()}')
print(f'dss.dss_num_classes(): {dss.num_classes()}')
print(f'dss.dss_num_user_classes(): {dss.num_user_classes()}')
print(f'dss.dss_reset(): {dss.reset()}')
print(f'dss.dss_read_allow_forms(): {dss.allow_forms_read()}')
print(f'dss.dss_write_allow_forms(): {dss.allow_forms_write(0)}')
print(f'dss.dss_read_allow_forms(): {dss.allow_forms_read()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.dss_new_circuit(): {dss.new_circuit("new_rest_circuit")}')
print(f'dss.dss_version(): {dss.version()}')

print(f'dss.dss_read_datapath(): {dss.datapath_read()}')
# PAY ATTENTION: According with the OpenDSS original source there is no error here,
dss.datapath_write(r"C:\Users\eniocc\Desktop\epri_projects\fork\py_dss_interface\src\py_dss_interface\models"
                   r"\Capacitors\CapacitorsS.py")
print(f'dss.dss_read_datapath(): {dss.datapath_read()}')
print(f'dss.dss_default_editor(): {dss.default_editor()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.dss_classes(): {dss.classes()}')
print(f'dss.dss_user_classes(): {dss.user_classes()}')
