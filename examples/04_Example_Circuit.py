# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.circuit_num_ckt_elements(): {dss.circuit_num_ckt_elements()}')
print(f'dss.circuit_num_buses(): {dss.circuit_num_buses()}')
print(f'dss.circuit_num_nodes(): {dss.circuit_num_nodes()}')
print(f'dss.circuit_first_pc_element(): {dss.circuit_first_pc_element()}')
print(f'dss.circuit_next_pc_element(): {dss.circuit_next_pc_element()}')
print(f'dss.circuit_first_pd_element(): {dss.circuit_first_pd_element()}')
print(f'dss.circuit_next_pd_element(): {dss.circuit_next_pd_element()}')
print(f'dss.circuit_sample(): {dss.circuit_sample()}')

print(f'dss.circuit_set_active_bus_i(): {dss.circuit_set_active_bus_i(1)}')
print(f'dss.circuit_first_element(): {dss.circuit_first_element()}')
print(f'dss.circuit_next_element(): {dss.circuit_next_element()}')

print(f'dss.circuit_update_storage_t(): {dss.circuit_update_storage_t()}')
print(f'dss.circuit_parent_pd_element(): {dss.circuit_parent_pd_element()}')
print(f'dss.circuit_end_of_time_step_update(): {dss.circuit_end_of_time_step_update()}')
# To iterate from begin we must call first()
# dss.active_class_first()
# for i in range(dss.active_class_num_elements()):
#     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
#     dss.active_class_next()


# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.circuit_capacity(): {dss.circuit_capacity()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.circuit_name(): {dss.circuit_name()}')
print(f'dss.circuit_disable(): {dss.circuit_disable()}')
print(f'dss.circuit_enable(): {dss.circuit_enable()}')

print(f'dss.circuit_set_active_element(): {dss.circuit_set_active_element("Line.650632")}')
print(f'dss.active_class_get_name(): {dss.get_name()}')

print(f'dss.circuit_set_active_bus(): {dss.circuit_set_active_bus("692")}')

print(f'circuit_set_active_class(): {dss.circuit_set_active_class("Capacitor")}')
print(f'dss.active_class_get_name(): {dss.get_name()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.circuit_losses(): {dss.circuit_losses()}')
print(f'dss.circuit_line_losses(): {dss.circuit_line_losses()}')
print(f'dss.circuit_substation_losses(): {dss.circuit_substation_losses()}')
print(f'dss.circuit_total_power(): {dss.circuit_total_power()}')
print(f'dss.circuit_all_bus_volts(): {dss.circuit_all_bus_volts()}')
print(f'dss.circuit_all_bus_vmag(): {dss.circuit_all_bus_vmag()}')
print(f'dss.circuit_all_element_names(): {dss.circuit_all_element_names()}')
print(f'dss.circuit_all_bus_names(): {dss.circuit_all_bus_names()}')
print(f'dss.circuit_all_element_losses(): {dss.circuit_all_element_losses()}')
print(f'dss.circuit_all_bus_vmag_pu(): {dss.circuit_all_bus_vmag_pu()}')
print(f'dss.circuit_all_node_names(): {dss.circuit_all_node_names()}')
print(f'dss.circuit_system_y(): {dss.circuit_system_y()}')
print(f'dss.circuit_all_bus_distances(): {dss.circuit_all_bus_distances()}')
print(f'dss.circuit_all_node_distances(): {dss.circuit_all_node_distances()}')
print(f'dss.circuit_all_node_vmag_by_phase(): {dss.circuit_all_node_vmag_by_phase(2)}')
print(f'dss.circuit_all_node_vmag_pu_by_phase(): {dss.circuit_all_node_vmag_pu_by_phase(2)}')
print(f'dss.circuit_all_node_distances_by_phase(): {dss.circuit_all_node_distances_by_phase(3)}')
print(f'dss.circuit_all_node_names_by_phase(): {dss.circuit_all_node_names_by_phase(2)}')
print(f'dss.circuit_y_node_varray(): {dss.circuit_y_node_varray()}')
print(f'dss.circuit_y_node_order(): {dss.circuit_y_node_order()}')
print(f'dss.circuit_y_currents(): {dss.circuit_y_currents()}')
