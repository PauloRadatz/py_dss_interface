# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods ' + 45 * '=')
print(f'dss.capcontrols_count(). {dss.capcontrols_count()}')
print(f'dss.capcontrols_first(). {dss.capcontrols_first()}')
print(f'dss.capcontrols_read_name(). {dss.capcontrols_read_name()}')
print(f'dss.capcontrols_next(). {dss.capcontrols_next()}')
print(f'dss.capcontrols_read_mode(). {dss.capcontrols_read_mode()}')
print(f'dss.capcontrols_write_mode(). {dss.capcontrols_write_mode(1)}')
print(f'dss.capcontrols_read_mode(). {dss.capcontrols_read_mode()}')
print(f'dss.capcontrols_read_monitored_term(). {dss.capcontrols_read_monitored_term()}')
print(f'dss.capcontrols_write_monitored_term(). {dss.capcontrols_write_monitored_term(dss, 1)}')
print(f'dss.capcontrols_read_use_volt_override(). {dss.capcontrols_read_use_volt_override()}')
print(f'dss.capcontrols_write_use_volt_override(). {dss.capcontrols_write_use_volt_override(dss, 1)}')
print(f'dss.capcontrols_read_use_volt_override(). {dss.capcontrols_read_use_volt_override()}')

# To iterate from begin we must call first()
# dss.active_class_first()
# for i in range(dss.active_class_num_elements()):
#     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
#     dss.active_class_next()

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.capcontrols_read_ct_ratio(). {dss.capcontrols_read_ct_ratio()}')
print(f'dss.capcontrols_write_ct_ratio(). {dss.capcontrols_write_ct_ratio(1)}')
print(f'dss.capcontrols_read_ct_ratio(). {dss.capcontrols_read_ct_ratio()}')

print(f'dss.capcontrols_read_pt_ratio(). {dss.capcontrols_read_pt_ratio()}')
print(f'dss.capcontrols_write_pt_ratio(). {dss.capcontrols_write_pt_ratio(1)}')
print(f'dss.capcontrols_read_pt_ratio(). {dss.capcontrols_read_pt_ratio()}')

print(f'dss.capcontrols_read_on_setting(). {dss.capcontrols_read_on_setting()}')
print(f'dss.capcontrols_write_on_setting(). {dss.capcontrols_write_on_setting(12.52)}')
print(f'dss.capcontrols_read_on_setting(). {dss.capcontrols_read_on_setting()}')

print(f'dss.capcontrols_read_off_setting(). {dss.capcontrols_read_off_setting()}')
print(f'dss.capcontrols_write_off_setting(). {dss.capcontrols_write_off_setting(12.34)}')
print(f'dss.capcontrols_read_off_setting(). {dss.capcontrols_read_off_setting()}')

print(f'dss.capcontrols_read_vmax(). {dss.capcontrols_read_vmax()}')
print(f'dss.capcontrols_write_vmax(). {dss.capcontrols_write_vmax(12.58)}')
print(f'dss.capcontrols_read_vmax(). {dss.capcontrols_read_vmax()}')

print(f'dss.capcontrols_read_vmin(). {dss.capcontrols_read_vmin()}')
print(f'dss.capcontrols_write_vmax(). {dss.capcontrols_write_vmin(12.58)}')
print(f'dss.capcontrols_read_vmin(). {dss.capcontrols_read_vmin()}')

print(f'dss.capcontrols_read_delay(). {dss.capcontrols_read_delay()}')
print(f'dss.capcontrols_write_delay(). {dss.capcontrols_write_delay(12.68)}')
print(f'dss.capcontrols_read_delay(). {dss.capcontrols_read_delay()}')

print(f'dss.capcontrols_read_delay_off(). {dss.capcontrols_read_delay_off()}')
print(f'dss.capcontrols_write_delay_off(). {dss.capcontrols_write_delay_off(12.68)}')
print(f'dss.capcontrols_read_delay_off(). {dss.capcontrols_read_delay_off()}')

print(f'dss.capcontrols_read_dead_time(). {dss.capcontrols_read_dead_time()}')
print(f'dss.capcontrols_write_dead_time(). {dss.capcontrols_write_dead_time(12.68)}')
print(f'dss.capcontrols_read_dead_time(). {dss.capcontrols_read_dead_time()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.capcontrols_read_name(). {dss.capcontrols_read_name()}')
# print(f'dss.capcontrols_write_name(). {dss.capcontrols_write_name("cogumelo")}')
print(f'dss.capcontrols_read_name(). {dss.capcontrols_read_name()}')

print(f'dss.capcontrols_read_capacitor(). {dss.capcontrols_read_capacitor()}')
print(f'dss.capcontrols_write_capacitor(). {dss.capcontrols_write_capacitor("random_capacitor_name")}')
print(f'dss.capcontrols_read_capacitor(). {dss.capcontrols_read_capacitor()}')

print(f'dss.capcontrols_read_monitored_obj(). {dss.capcontrols_read_monitored_obj()}')
print(f'dss.capcontrols_write_monitored_obj(). {dss.capcontrols_write_monitored_obj("random_capacitor_name")}')
print(f'dss.capcontrols_read_monitored_obj(). {dss.capcontrols_read_monitored_obj()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.capcontrols_all_names(). {dss.capcontrols_all_names()}')
