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
print(45 * '=' + ' Integer Methods ' + 45 * '=')
print(f'dss.bus_num_nodes(): {dss.bus_num_nodes()}')
print(f'dss.bus_zsc_refresh(): {dss.bus_zsc_refresh()}')
print(f'dss.bus_coord_defined(): {dss.bus_coord_defined()}')
print(f'dss.bus_get_unique_node_number(): {dss.bus_get_unique_node_number()}')
print(f'dss.bus_total_customers(): {dss.bus_total_customers()}')
print(f'dss.bus_section_id(): {dss.bus_section_id()}')
print()

# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.bus_kv_base(): {dss.bus_kv_base()}')

print(f'dss.bus_read_x(): {dss.bus_read_x()}')
print(f'dss.bus_write_x(): {dss.bus_write_x(35.921882)}')
print(f'dss.bus_read_x(): {dss.bus_read_x()}')

print(f'dss.bus_read_y(): {dss.bus_read_y()}')
print(f'dss.bus_write_y(): {dss.bus_write_y(-84.141987)}')
print(f'dss.bus_read_y(): {dss.bus_read_y()}')

print(f'dss.bus_distance(): {dss.bus_distance()}')
print(f'dss.bus_accumulated_failure_rate(): {dss.bus_accumulated_failure_rate()}')
print()
print(f'dss.bus_interruptions_num(): {dss.bus_interruptions_num()}')
print(f'dss.bus_interruptions_avg_duration(): {dss.bus_interruptions_avg_duration()}')
print(f'dss.bus_interruptions_total_customers_(): {dss.bus_interruptions_total_customers_()}')
print(f'dss.bus_outage_customer_accum_duration(): {dss.bus_outage_customer_accum_duration()}')
print(f'dss.bus_line_total_miles(): {dss.bus_line_total_miles()}')
print()
print(f'dss.bus_latitude_read(): {dss.bus_latitude_read()}')
print(f'dss.bus_latitude_write(): {dss.bus_latitude_write(35.921882)}')
print(f'dss.bus_latitude_read(): {dss.bus_latitude_read()}')
print()
print(f'dss.bus_latitude_read(): {dss.bus_longitude_read()}')
print(f'dss.bus_latitude_read(): {dss.bus_longitude_write(-84.141987)}')
print(f'dss.bus_latitude_read(): {dss.bus_longitude_read()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'dss.bus_name(): {dss.bus_name()}')

# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.bus_voltages(): {dss.bus_voltages()}')
print(f'dss.bus_seqvoltages(): {dss.bus_seqvoltages()}')
print(f'dss.bus_nodes(): {dss.bus_nodes()}')
print(f'dss.bus_voc(): {dss.bus_voc()}')
print(f'dss.bus_isc(): {dss.bus_isc()}')
print(f'dss.bus_pu_voltages(): {dss.bus_pu_voltages()}')
print(f'dss.bus_zsc_matrix(): {dss.bus_zsc_matrix()}')
print(f'dss.bus_zsc1(): {dss.bus_zsc1()}')
print(f'dss.bus_zsc0(): {dss.bus_zsc0()}')
print(f'dss.bus_ysc_matrix(): {dss.bus_ysc_matrix()}')
print(f'dss.bus_sequence_voltages(): {dss.bus_sequence_voltages()}')

print(f'dss.bus_vll(): {dss.bus_vll()}')
print(f'dss.bus_pu_vll(): {dss.bus_pu_vll()}')

print(f'dss.bus_vmag_angle(): {dss.bus_vmag_angle()}')
print(f'dss.bus_pu_vmag_angle(): {dss.bus_pu_vmag_angle()}')

print(f'dss.bus_line_list(): {dss.bus_line_list()}')

print(f'dss.bus_load_list(): {dss.bus_load_list()}')

print(f'dss.bus_axc_012_matrix(): {dss.bus_axc_012_matrix()}')
print(f'dss.bus_all_pce_active_bus(): {dss.bus_all_pce_active_bus()}')
print(f'dss.bus_all_pde_active_bus(): {dss.bus_all_pde_active_bus()}')
