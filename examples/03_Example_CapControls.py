# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods ' + 45 * '=')
print(f'dss.capcontrols_count(). {dss._count()}')
print(f'dss.capcontrols_first(). {dss.first()}')
print(f'dss.capcontrols_read_name(). {dss.read_name()}')
print(f'dss.capcontrols_next(). {dss.next()}')
print(f'dss.capcontrols_read_mode(). {dss.read_mode()}')
print(f'dss.capcontrols_write_mode(). {dss.write_mode(1)}')
print(f'dss.capcontrols_read_mode(). {dss.read_mode()}')
print(f'dss.capcontrols_read_monitored_term(). {dss.read_monitored_term()}')
print(f'dss.capcontrols_write_monitored_term(). {dss.write_monitored_term(dss, 1)}')
print(f'dss.capcontrols_read_use_volt_override(). {dss.read_use_volt_override()}')
print(f'dss.capcontrols_write_use_volt_override(). {dss.write_use_volt_override(dss, 1)}')
print(f'dss.capcontrols_read_use_volt_override(). {dss.read_use_volt_override()}')

# To iterate from begin we must call first()
# dss.active_class_first()
# for i in range(dss.active_class_num_elements()):
#     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
#     dss.active_class_next()

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.capcontrols_read_ct_ratio(). {dss.ct_ratio_read()}')
print(f'dss.capcontrols_write_ct_ratio(). {dss.ct_ratio_write(1)}')
print(f'dss.capcontrols_read_ct_ratio(). {dss.ct_ratio_read()}')

print(f'dss.capcontrols_read_pt_ratio(). {dss.pt_ratio_read()}')
print(f'dss.capcontrols_write_pt_ratio(). {dss.pt_ratio_write(1)}')
print(f'dss.capcontrols_read_pt_ratio(). {dss.pt_ratio_read()}')

print(f'dss.capcontrols_read_on_setting(). {dss.on_setting_read()}')
print(f'dss.capcontrols_write_on_setting(). {dss.on_setting_write(12.52)}')
print(f'dss.capcontrols_read_on_setting(). {dss.on_setting_read()}')

print(f'dss.capcontrols_read_off_setting(). {dss.off_setting_read()}')
print(f'dss.capcontrols_write_off_setting(). {dss.off_setting_write(12.34)}')
print(f'dss.capcontrols_read_off_setting(). {dss.off_setting_read()}')

print(f'dss.capcontrols_read_vmax(). {dss.vmax_read()}')
print(f'dss.capcontrols_write_vmax(). {dss.vmax_write(12.58)}')
print(f'dss.capcontrols_read_vmax(). {dss.vmax_read()}')

print(f'dss.capcontrols_read_vmin(). {dss.vmin_read()}')
print(f'dss.capcontrols_write_vmax(). {dss.vmin_write(12.58)}')
print(f'dss.capcontrols_read_vmin(). {dss.vmin_read()}')

print(f'dss.capcontrols_read_delay(). {dss.delay_read()}')
print(f'dss.capcontrols_write_delay(). {dss.delay_write(12.68)}')
print(f'dss.capcontrols_read_delay(). {dss.delay_read()}')

print(f'dss.capcontrols_read_delay_off(). {dss.delay_off_read()}')
print(f'dss.capcontrols_write_delay_off(). {dss.delay_off_write(12.68)}')
print(f'dss.capcontrols_read_delay_off(). {dss.delay_off_read()}')

print(f'dss.capcontrols_read_dead_time(). {dss.dead_time_read()}')
print(f'dss.capcontrols_write_dead_time(). {dss.dead_time_write(12.68)}')
print(f'dss.capcontrols_read_dead_time(). {dss.dead_time_read()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.capcontrols_read_name(). {dss.read_name()}')
# print(f'dss.capcontrols_write_name(). {dss.capcontrols_write_name("cogumelo")}')
print(f'dss.capcontrols_read_name(). {dss.read_name()}')

print(f'dss.capcontrols_read_capacitor(). {dss.capacitor_controlled()}')
print(f'dss.capcontrols_write_capacitor(). {dss.write_capacitor("random_capacitor_name")}')
print(f'dss.capcontrols_read_capacitor(). {dss.capacitor_controlled()}')

print(f'dss.capcontrols_read_monitored_obj(). {dss.monitored_object()}')
print(f'dss.capcontrols_write_monitored_obj(). {dss.monitored_obj_write("random_capacitor_name")}')
print(f'dss.capcontrols_read_monitored_obj(). {dss.monitored_object()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.capcontrols_all_names(). {dss.all_names()}')
