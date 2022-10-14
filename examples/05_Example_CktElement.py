# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'cktelement_num_terminals: {dss.num_terminals()}')
print(f'cktelement_num_conductors: {dss.num_conductors()}')
print(f'cktelement_num_phases: {dss.num_phases()}')
print(f'cktelement_open: {dss.open_terminal(1)}')
print(f'cktelement_is_open: {dss.is_terminal_open()}')
print(f'cktelement_close: {dss.close_terminal(1)}')
print(f'cktelement_is_open: {dss.is_terminal_open()}')
print(f'cktelement_open: {dss.open_terminal(1)}')
print(f'cktelement_is_open: {dss.is_terminal_open()}')
print(f'cktelement_num_properties: {dss.num_properties()}')
print(f'cktelement_has_switch_control: {dss.has_switch_control()}')
print(f'cktelement_has_volt_control: {dss.has_volt_control()}')
print(f'cktelement_num_controls: {dss.num_controls()}')
dss.set_active_element('Line.650632')
print(f'cktelement_num_controls: {dss.num_controls()}')
print(f'cktelement_ocp_dev_index: {dss.ocp_dev_index()}')
print(f'cktelement_ocp_dev_type: {dss.ocp_dev_type()}')
print(f'cktelement_read_enabled: {dss.is_enabled()}')
print(f'cktelement_write_enabled: {dss.enabled(0)}')
print(f'cktelement_read_enabled: {dss.is_enabled()}')
#
# # To iterate from begin we must call first()
# # dss.active_class_first()
# # for i in range(dss.active_class_num_elements()):
# #     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
# #     dss.active_class_next()
#
# # Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'cktelement_read_norm_amps: {dss.norm_amps_read()}')
print(f'cktelement_write_norm_amps: {dss.norm_amps_write(12.0)}')
print(f'cktelement_read_norm_amps: {dss.norm_amps_read()}')
#
print(f'cktelement_read_emerg_amps: {dss.emerg_amps_read()}')
print(f'cktelement_write_emerg_amp: {dss.emerg_amps_write(23)}')
print(f'cktelement_read_emerg_amps: {dss.emerg_amps_read()}')
#
print(f'cktelement_variable_i: {dss.variable_i(1)}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'cktelement_name: {dss.name()}')

print(f'cktelement_read_display: {dss.display_read()}')
print(f'cktelement_write_display: {dss.display_write("jurubeba")}')
print(f'cktelement_read_display: {dss.display_read()}')
print(f'cktelement_write_display: {dss.display_write("Line_671692")}')
print(f'cktelement_read_display: {dss.display_read()}')

print(f'cktelement_guid: {dss.guid()}')

dss.set_active_element('Line.650632')
print(f'cktelement_energymeter: {dss.energymeter()}')
# Adding an element control
dss.text("New 'Fuse.f1' MonitoredObj=Line.650632 MonitoredTerm=1 FuseCurve=Klink RatedCurrent=65")
# After include a new element it become the active element. So, we need activate another element to test the methods
# below
dss.set_active_element('Line.650632')
print(f'cktelement_controller: {dss.controller("1")}')


# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'cktelement_read_bus_names: {dss.read_bus_names()}')
print(f'cktelement_write_bus_names: {dss.write_bus_names(dss, ["NovoNomeBus1", "NovoNomeBus2"])}')
print(f'cktelement_read_bus_names: {dss.read_bus_names()}')
print(f'cktelement_write_bus_names: {dss.write_bus_names(dss, ["671", "692"])}')
print(f'cktelement_read_bus_names: {dss.read_bus_names()}')

# Before set a specific active element
print(f'cktelement_voltages: {dss.voltages()}')
print(f'cktelement_currents: {dss.currents()}')
print(f'cktelement_powers: {dss.powers()}')
print(f'cktelement_losses: {dss.losses()}')

# After set a specific active element
dss.set_active_element("Load.671")
print(f'cktelement_voltages: {dss.voltages()}')
print(f'cktelement_currents: {dss.currents()}')

print(f'cktelement_powers: {dss.powers()}')
print(f'cktelement_losses: {dss.losses()}')
print(f'cktelement_phase_losses: {dss.phase_losses()}')
print(f'cktelement_seq_voltages: {dss.seq_voltages()}')
print(f'cktelement_seq_currents: {dss.seq_currents()}')
print(f'cktelement_seq_powers: {dss.seq_powers()}')
print(f'cktelement_all_property_names: {dss.property_names()}')
print(f'cktelement_residuals: {dss.residuals_currents()}')
print(f'cktelement_y_prim: {dss.y_prim()}')
print(f'cktelement_cplx_seq_voltages: {dss.cplx_seq_voltages()}')
print(f'cktelement_cplx_seq_currents: {dss.cktelement_cplx_seq_currents()}')
print(f'cktelement_node_order: {dss.node_order()}')
print(f'cktelement_currents_mag_ang: {dss.currents_mag_ang()}')
print(f'cktelement_voltages_mag_ang: {dss.voltages_mag_ang()}')
#
# dss.circuit_set_active_element("Line.632633")
dss.text("Edit Load.692")
print(f'cktelement_name: {dss.name()}')
print(f'cktelement_all_variables_names: {dss.variables_names()}')
print(f'cktelement_all_variables_values: {dss.variables_values()}')

