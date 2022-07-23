# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss


dss.circuit_set_active_bus('692')
print(dss.name())

# # Integer methods
# print(45 * '=' + ' Integer Methods ' + 45 * '=')
# print(f'dss.bus_num_nodes(): {dss.bus_num_nodes()}')
# print(f'dss.bus_zsc_refresh(): {dss.bus_zsc_refresh()}')
# print(f'dss.bus_coord_defined(): {dss.bus_coord_defined()}')
# print(f'dss.bus_get_unique_node_number(): {dss.bus_get_unique_node_number()}')
# print(f'dss.bus_total_customers(): {dss.bus_total_customers()}')
# print(f'dss.bus_section_id(): {dss.bus_section_id()}')
# print()
#
# # Float methods
# print(45 * '=' + ' Float Methods ' + 45 * '=')
# print(f'dss.bus_kv_base(): {dss.bus_kv_base()}')
#
# print(f'dss.bus_read_x(): {dss.bus_read_x()}')
# print(f'dss.bus_write_x(): {dss.bus_write_x(35.921882)}')
# print(f'dss.bus_read_x(): {dss.bus_read_x()}')
#
# print(f'dss.bus_read_y(): {dss.bus_read_y()}')
# print(f'dss.bus_write_y(): {dss.bus_write_y(-84.141987)}')
# print(f'dss.bus_read_y(): {dss.bus_read_y()}')
#
# print(f'dss.bus_distance(): {dss.bus_distance()}')
# print(f'dss.bus_accumulated_failure_rate(): {dss.bus_accumulated_failure_rate()}')
# print()
# print(f'dss.bus_interruptions_num(): {dss.bus_interruptions_num()}')
# print(f'dss.bus_interruptions_avg_duration(): {dss.bus_interruptions_avg_duration()}')
# print(f'dss.bus_interruptions_total_customers_(): {dss.bus_interruptions_total_customers_()}')
# print(f'dss.bus_outage_customer_accum_duration(): {dss.bus_outage_customer_accum_duration()}')
# print(f'dss.bus_line_total_miles(): {dss.bus_line_total_miles()}')
# print()
# print(f'dss.bus_latitude_read(): {dss.bus_latitude_read()}')
# print(f'dss.bus_latitude_write(): {dss.bus_latitude_write(35.921882)}')
# print(f'dss.bus_latitude_read(): {dss.bus_latitude_read()}')
# print()
# print(f'dss.bus_longitude_read(): {dss.bus_longitude_read()}')
# print(f'dss.bus_longitude_read(): {dss.bus_longitude_write(-84.141987)}')
# print(f'dss.bus_longitude_read(): {dss.bus_longitude_read()}')
#
# # String methods
# print(45 * '=' + ' String Methods ' + 45 * '=')
# print(f'dss.bus_name(): {dss.bus_name()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.bus_voltages(): {dss.voltages()}')
print(f'dss.bus_seqvoltages(): {dss.seq_voltages()}')
print(f'dss.bus_nodes(): {dss.nodes()}')
print(f'dss.bus_voc(): {dss.voc()}')
print(f'dss.bus_isc(): {dss.isc()}')
print(f'dss.bus_pu_voltages(): {dss.pu_voltages()}')
print(f'dss.bus_zsc_matrix(): {dss.zsc_matrix()}')
print(f'dss.bus_zsc1(): {dss.zsc1()}')
print(f'dss.bus_zsc0(): {dss.zsc0()}')
print(f'dss.bus_ysc_matrix(): {dss.ysc_matrix()}')
print(f'dss.bus_sequence_voltages(): {dss.bus_sequence_voltages()}')

print(f'dss.bus_vll(): {dss.vll()}')
print(f'dss.bus_pu_vll(): {dss.pu_vll()}')

print(f'dss.bus_vmag_angle(): {dss.vmag_angle()}')
print(f'dss.bus_pu_vmag_angle(): {dss.vmag_angle_pu()}')

print(f'dss.bus_line_list(): {dss.line_list()}')

print(f'dss.bus_load_list(): {dss.load_list()}')

print(f'dss.bus_axc_012_matrix(): {dss.axc_012_matrix()}')
print(f'dss.bus_all_pce_active_bus(): {dss.all_pce_active_bus()}')
print(f'dss.bus_all_pde_active_bus(): {dss.all_pde_active_bus()}')
