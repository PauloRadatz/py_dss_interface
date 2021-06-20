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
print(45 * '=' + ' Integer Methods ' + 45 * '=')
print(f'dss.cap_controls_count(). {dss.cap_controls_count()}')
print(f'dss.cap_controls_first(). {dss.cap_controls_first()}')
print(f'dss.cap_controls_read_name(). {dss.cap_controls_read_name()}')
print(f'dss.cap_controls_next(). {dss.cap_controls_next()}')
print(f'dss.cap_controls_read_mode(). {dss.cap_controls_read_mode()}')
print(f'dss.cap_controls_write_mode(). {dss.cap_controls_write_mode(1)}')
print(f'dss.cap_controls_read_mode(). {dss.cap_controls_read_mode()}')
print(f'dss.cap_controls_read_monitored_term(). {dss.cap_controls_read_monitored_term()}')
print(f'dss.cap_controls_write_monitored_term(). {dss.cap_controls_write_monitored_term(dss, 1)}')
print(f'dss.cap_controls_read_use_volt_override(). {dss.cap_controls_read_use_volt_override()}')
print(f'dss.cap_controls_write_use_volt_override(). {dss.cap_controls_write_use_volt_override(dss, 1)}')
print(f'dss.cap_controls_read_use_volt_override(). {dss.cap_controls_read_use_volt_override()}')

# To iterate from begin we must call first()
# dss.active_class_first()
# for i in range(dss.active_class_num_elements()):
#     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
#     dss.active_class_next()

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.cap_controls_read_ct_ratio(). {dss.cap_controls_read_ct_ratio()}')
print(f'dss.cap_controls_write_ct_ratio(). {dss.cap_controls_write_ct_ratio(1)}')
print(f'dss.cap_controls_read_ct_ratio(). {dss.cap_controls_read_ct_ratio()}')

print(f'dss.cap_controls_read_pt_ratio(). {dss.cap_controls_read_pt_ratio()}')
print(f'dss.cap_controls_write_pt_ratio(). {dss.cap_controls_write_pt_ratio(1)}')
print(f'dss.cap_controls_read_pt_ratio(). {dss.cap_controls_read_pt_ratio()}')

print(f'dss.cap_controls_read_on_setting(). {dss.cap_controls_read_on_setting()}')
print(f'dss.cap_controls_write_on_setting(). {dss.cap_controls_write_on_setting(12.52)}')
print(f'dss.cap_controls_read_on_setting(). {dss.cap_controls_read_on_setting()}')

print(f'dss.cap_controls_read_off_setting(). {dss.cap_controls_read_off_setting()}')
print(f'dss.cap_controls_write_off_setting(). {dss.cap_controls_write_off_setting(12.34)}')
print(f'dss.cap_controls_read_off_setting(). {dss.cap_controls_read_off_setting()}')

print(f'dss.cap_controls_read_vmax(). {dss.cap_controls_read_vmax()}')
print(f'dss.cap_controls_write_vmax(). {dss.cap_controls_write_vmax(12.58)}')
print(f'dss.cap_controls_read_vmax(). {dss.cap_controls_read_vmax()}')

print(f'dss.cap_controls_read_vmin(). {dss.cap_controls_read_vmin()}')
print(f'dss.cap_controls_write_vmax(). {dss.cap_controls_write_vmin(12.58)}')
print(f'dss.cap_controls_read_vmin(). {dss.cap_controls_read_vmin()}')

print(f'dss.cap_controls_read_delay(). {dss.cap_controls_read_delay()}')
print(f'dss.cap_controls_write_delay(). {dss.cap_controls_write_delay(12.68)}')
print(f'dss.cap_controls_read_delay(). {dss.cap_controls_read_delay()}')

print(f'dss.cap_controls_read_delay_off(). {dss.cap_controls_read_delay_off()}')
print(f'dss.cap_controls_write_delay_off(). {dss.cap_controls_write_delay_off(12.68)}')
print(f'dss.cap_controls_read_delay_off(). {dss.cap_controls_read_delay_off()}')

print(f'dss.cap_controls_read_dead_time(). {dss.cap_controls_read_dead_time()}')
print(f'dss.cap_controls_write_dead_time(). {dss.cap_controls_write_dead_time(12.68)}')
print(f'dss.cap_controls_read_dead_time(). {dss.cap_controls_read_dead_time()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.cap_controls_read_name(). {dss.cap_controls_read_name()}')
# print(f'dss.cap_controls_write_name(). {dss.cap_controls_write_name("cogumelo")}')
print(f'dss.cap_controls_read_name(). {dss.cap_controls_read_name()}')

print(f'dss.cap_controls_read_capacitor(). {dss.cap_controls_read_capacitor()}')
print(f'dss.cap_controls_write_capacitor(). {dss.cap_controls_write_capacitor("random_capacitor_name")}')
print(f'dss.cap_controls_read_capacitor(). {dss.cap_controls_read_capacitor()}')

print(f'dss.cap_controls_read_monitored_obj(). {dss.cap_controls_read_monitored_obj()}')
print(f'dss.cap_controls_write_monitored_obj(). {dss.cap_controls_write_monitored_obj("random_capacitor_name")}')
print(f'dss.cap_controls_read_monitored_obj(). {dss.cap_controls_read_monitored_obj()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.cap_controls_all_names(). {dss.cap_controls_all_names()}')
