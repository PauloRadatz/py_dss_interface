# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.generators_first(): {dss.first()}')
print(f'dss.generators_next(): {dss.next()}')
print(f'dss.generators_read_forced_on(): {dss.forced_on_read()}')
print(f'dss.generators_write_forced_on(): {dss.forced_on_write(1)}')
print(f'dss.generators_read_phases(): {dss.phases_read()}')
print(f'dss.generators_write_phases(): {dss.phases_write(2)}')
print(f'dss.generators_count(): {dss._count()}')
print(f'dss.generators_read_idx(): {dss.idx_read()}')
print(f'dss.generators_write_idx(): {dss.idx_write(1)}')

# PAY ATTENTION: -1 is the output with there is no generators in your case
print(f'dss.generators_read_model(): {dss.model_read()}')
print(f'dss.generators_write_model(): {dss.model_write(3)}')
print(f'dss.generators_read_model(): {dss.model_read()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.generators_read_name(): {dss.name_read()}')
# PAY ATTENTION: If no generator, a error message will pop up in your screen
print(f'dss.generators_write_name(): {dss.name_write("My Generator")}')
print(f'dss.generators_read_name(): {dss.name_read()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.generators_read_kv(): {dss.kv_read()}')
print(f'dss.generators_write_kv(): {dss.kv_write(12.3)}')
print(f'dss.generators_read_kv(): {dss.kv_read()}')
print(f'dss.generators_read_kw(): {dss.kw_read()}')
print(f'dss.generators_write_kw(): {dss.kw_write(15.8)}')
print(f'dss.generators_read_kw(): {dss.kw_read()}')
print(f'dss.generators_read_kvar(): {dss.kvar_read()}')
print(f'dss.generators_write_kvar(): {dss.kvar_write(12.68)}')
print(f'dss.generators_read_kvar(): {dss.kvar_read()}')
print(f'dss.generators_read_pf(): {dss.pf_read()}')
print(f'dss.generators_write_pf(): {dss.pf_write(15.32)}')
print(f'dss.generators_read_pf(): {dss.pf_read()}')
print(f'dss.generators_read_kva_rated(): {dss.kva_rated_read()}')
print(f'dss.generators_write_kva_rated(): {dss.kva_rated_write(123)}')
print(f'dss.generators_read_kva_rated(): {dss.kva_rated_read()}')
print(f'dss.generators_read_vmax_pu(): {dss.generators_read_vmax_pu()}')
print(f'dss.generators_write_vmax_pu(): {dss.generators_write_vmax_pu(32)}')
print(f'dss.generators_read_vmax_pu(): {dss.generators_read_vmax_pu()}')
print(f'dss.generators_read_vmin_pu(): {dss.generators_read_vmin_pu()}')
print(f'dss.generators_write_vmin_pu(): {dss.generators_write_vmin_pu(54)}')
print(f'dss.generators_read_vmin_pu(): {dss.generators_read_vmin_pu()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.generators_all_names(): {dss.names()}')
print(f'dss.generators_register_names(): {dss.register_names()}')
print(f'dss.generators_register_values(): {dss.register_values()}')
