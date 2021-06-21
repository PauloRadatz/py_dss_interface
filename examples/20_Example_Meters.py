# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.meters_first(): {dss.meters_first()}')
print(f'dss.meters_next(): {dss.meters_next()}')
print(f'dss.meters_first(): {dss.meters_first()}')
print(f'dss.meters_reset(): {dss.meters_reset()}')
print(f'dss.meters_reset_all(): {dss.meters_reset_all()}')
print(f'dss.meters_sample(): {dss.meters_sample()}')
print(f'dss.meters_save(): {dss.meters_save()}')
print(f'dss.meters_read_metered_terminal(): {dss.meters_read_metered_terminal()}')
print(f'dss.meters_write_metered_terminal(): {dss.meters_write_metered_terminal(1)}')
print(f'dss.meters_di_files_are_open(): {dss.meters_di_files_are_open()}')
# print(f'dss.meters_sample_all(): {dss.meters_sample_all()}')
print(f'dss.meters_save_all(): {dss.meters_save_all()}')
print(f'dss.meters_open_all_di_files(): {dss.meters_open_all_di_files()}')
print(f'dss.meters_close_all_di_files(): {dss.meters_close_all_di_files()}')
# print(f'dss.meters_count_end_elements(): {dss.meters_count_end_elements()}')
print(f'dss.meters_count(): {dss.meters_count()}')
print(f'dss.meters_count_branches(): {dss.meters_count_branches()}')
print(f'dss.meters_read_sequence_index(): {dss.meters_read_sequence_index()}')
print(f'dss.meters_write_sequence_index(): {dss.meters_write_sequence_index(2)}')
print(f'dss.meters_read_sequence_index(): {dss.meters_read_sequence_index()}')
print(f'dss.meters_write_sequence_index(): {dss.meters_write_sequence_index(1)}')
print(f'dss.meters_do_reliability_calc(): {dss.meters_do_reliability_calc()}')
print(f'dss.meters_seq_list_size(): {dss.meters_seq_list_size()}')
print(f'dss.meters_total_customers(): {dss.meters_total_customers()}')
print(f'dss.meters_num_sections(): {dss.meters_num_sections()}')
print(f'dss.meters_set_active_section(): {dss.meters_set_active_section(1)}')
print(f'dss.meters_ocp_device_type(): {dss.meters_ocp_device_type()}')
print(f'dss.meters_num_sections(): {dss.meters_num_sections()}')
print(f'dss.meters_num_section_branches(): {dss.meters_num_section_branches()}')
print(f'dss.meters_sect_seq_idx(): {dss.meters_sect_seq_idx()}')
print(f'dss.meters_sect_total_cust(): {dss.meters_sect_total_cust()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.meters_read_name(): {dss.meters_read_name()}')
print(f'dss.meters_write_name(): {dss.meters_write_name("em1")}')
print(f'dss.meters_read_metered_element(): {dss.meters_read_metered_element()}')
print(f'dss.meters_write_metered_element(): {dss.meters_write_metered_element("Line.650632")}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.meters_saifi(): {dss.meters_saifi()}')
print(f'dss.meters_saifi_kw(): {dss.meters_saifi_kw()}')
print(f'dss.meters_cust_interrupts(): {dss.meters_cust_interrupts()}')
print(f'dss.meters_avg_repair_time(): {dss.meters_avg_repair_time()}')
print(f'dss.meters_fault_rate_x_repair_hrs(): {dss.meters_fault_rate_x_repair_hrs()}')
print(f'dss.meters_sum_branch_flt_rates(): {dss.meters_sum_branch_flt_rates()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')

print(f'dss.meters_allnames(): {dss.meters_all_names()}')
print(f'dss.meters_registernames(): {dss.meters_register_names()}')
print(f'dss.meters_registervalues(): {dss.meters_register_values()}')
print(f'dss.meters_totals(): {dss.meters_totals()}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peak_current()}')
print(f'dss.meters_write_peakcurrent(): {dss.meters_write_peak_current("[550,600,680]")}')
print(f'dss.meters_read_peakcurrent(): {dss.meters_read_peak_current()}')

print(f'dss.meters_read_calcurrent(): {dss.meters_read_cal_current()}')
print(f'dss.meters_write_calcurrent(): {dss.meters_write_calcurrent("[1., 2., 3.]")}')
print(f'dss.meters_read_calcurrent(): {dss.meters_read_cal_current()}')
#
# print(f'dss.meters_read_allocfactors(): {dss.meters_read_alloc_factors()}')
# print(f'dss.meters_write_allocfactors(): {dss.meters_write_alloc_factors("[0.1]")}')
#
# print(f'dss.meters_allendelements(): {dss.meters_all_end_elements()}')
# print(f'dss.meters_allbranchesinzone(): {dss.meters_all_branches_in_zone()}')
#
# print(f'dss.monitors_channel(): {dss.monitors_channel("2")}')
# print(f'dss.monitors_allnames(): {dss.monitors_all_names()}')
#
