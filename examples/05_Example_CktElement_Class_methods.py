# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 12/05/2021
"""
import os
import pathlib
from py_dss_interface import DSSDLL as DSS

dss = DSS()

my_path = pathlib.Path(__file__).parents[1] # one level above
my_path = os.path.join(my_path, r"tests\py_dss_interface\13Bus")
dss_file = os.path.join(my_path, "IEEE13Nodeckt.dss")

dss.text("compile {0}".format(dss_file))

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
# print(f'cktelement_name: {dss.cktelement_name()}')
# print(f'cktelement_num_terminals: {dss.cktelement_num_terminals()}')
# print(f'cktelement_num_conductors: {dss.cktelement_num_conductors()}')
# print(f'cktelement_num_phases: {dss.cktelement_num_phases()}')
# print(f'cktelement_open: {dss.cktelement_open(1)}')
# print(f'cktelement_close: {dss.cktelement_close(1)}')
# print(f'cktelement_is_open: {dss.cktelement_is_open()}')
# print(f'cktelement_num_properties: {dss.cktelement_num_properties()}')
# print(f'cktelement_has_switch_control: {dss.cktelement_has_switch_control()}')
# print(f'cktelement_has_volt_control: {dss.cktelement_has_volt_control()}')
# print(f'cktelement_num_controls: {dss.cktelement_num_controls()}')
# print(f'cktelement_ocp_dev_index: {dss.cktelement_ocp_dev_index()}')
# print(f'cktelement_ocp_dev_type: {dss.cktelement_ocp_dev_type()}')
# print(f'cktelement_read_enabled: {dss.cktelement_read_enabled()}')
# print(f'cktelement_write_enabled: {dss.cktelement_write_enabled(0)}')
# print(f'cktelement_read_enabled: {dss.cktelement_read_enabled()}')
#
# # To iterate from begin we must call first()
# # dss.active_class_first()
# # for i in range(dss.active_class_num_elements()):
# #     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
# #     dss.active_class_next()
#
# # Float methods
# print(45 * '=' + ' Float Methods ' + 45 * '=')
# print(f'cktelement_read_norm_amps: {dss.cktelement_read_norm_amps()}')
# print(f'cktelement_write_norm_amps: {dss.cktelement_write_norm_amps(12.0)}')
# print(f'cktelement_read_norm_amps: {dss.cktelement_read_norm_amps()}')
#
# print(f'cktelement_read_emerg_amps: {dss.cktelement_read_emerg_amps()}')
# print(f'cktelement_write_emerg_amp: {dss.cktelement_write_emerg_amps(23)}')
# print(f'cktelement_read_emerg_amps: {dss.cktelement_read_emerg_amps()}')
#
# print(f'cktelement_variable_i: {dss.cktelement_variable_i(1)}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'cktelement_name: {dss.cktelement_name()}')

# print(f'cktelement_read_display: {dss.cktelement_read_display()}')
# print(f'cktelement_write_display: {dss.cktelement_write_display("jurubeba")}')
# print(f'cktelement_read_display: {dss.cktelement_read_display()}')
#
# print(f'cktelement_guid: {dss.cktelement_guid()}')
# print(f'cktelement_energymeter: {dss.cktelement_energymeter()}')
# print(f'cktelement_controller: {dss.cktelement_controller(1)}')


# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'cktelement_read_bus_names: {dss.cktelement_read_bus_names()}')
print(f'cktelement_write_bus_names: {dss.cktelement_write_bus_names(dss,["NovoNomeBus1","NovoNomeBus2"])}')
print(f'cktelement_voltages: {dss.cktelement_voltages()}')
print(f'cktelement_currents: {dss.cktelement_currents()}')
print(f'cktelement_powers: {dss.cktelement_powers()}')
print(f'cktelement_losses: {dss.cktelement_losses()}')
print(f'cktelement_phase_losses: {dss.cktelement_phase_losses()}')
print(f'cktelement_seq_voltages: {dss.cktelement_seq_voltages()}')
print(f'cktelement_seq_currents: {dss.cktelement_seq_currents()}')
print(f'cktelement_seq_powers: {dss.cktelement_seq_powers()}')
print(f'cktelement_all_property_names: {dss.cktelement_all_property_names()}')
print(f'cktelement_residuals: {dss.cktelement_residuals()}')
print(f'cktelement_y_prim: {dss.cktelement_y_prim()}')
print(f'cktelement_cplx_seq_voltages: {dss.cktelement_cplx_seq_voltages()}')
print(f'cktelement_cplx_seq_currents: {dss.cktelement_cplx_seq_currents()}')
print(f'cktelement_node_order: {dss.cktelement_node_order()}')
print(f'cktelement_currents_mag_ang: {dss.cktelement_currents_mag_ang()}')
print(f'cktelement_voltages_mag_ang: {dss.cktelement_voltages_mag_ang()}')

dss.circuit_set_active_element('Load.671')
print(f'cktelement_name: {dss.cktelement_name()}')
print(f'cktelement_all_variables_names: {dss.cktelement_all_variables_names()}')
print(f'cktelement_all_variables_values: {dss.cktelement_all_variables_values()}')
