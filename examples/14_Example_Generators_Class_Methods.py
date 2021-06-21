# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.generators_first(): {dss.generators_first()}')
print(f'dss.generators_next(): {dss.generators_next()}')
print(f'dss.generators_read_forced_on(): {dss.generators_read_forced_on()}')
print(f'dss.generators_write_forced_on(): {dss.generators_write_forced_on(1)}')
print(f'dss.generators_read_phases(): {dss.generators_read_phases()}')
print(f'dss.generators_write_phases(): {dss.generators_write_phases(2)}')
print(f'dss.generators_count(): {dss.generators_count()}')
print(f'dss.generators_read_idx(): {dss.generators_read_idx()}')
print(f'dss.generators_write_idx(): {dss.generators_write_idx(1)}')

# PAY ATTENTION: -1 is the output with there is no generators in your case
print(f'dss.generators_read_model(): {dss.generators_read_model()}')
print(f'dss.generators_write_model(): {dss.generators_write_model(3)}')
print(f'dss.generators_read_model(): {dss.generators_read_model()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.generators_read_name(): {dss.generators_read_name()}')
# PAY ATTENTION: If no generator, a error message will pop up in your screen
print(f'dss.generators_write_name(): {dss.generators_write_name("My Generator")}')
print(f'dss.generators_read_name(): {dss.generators_read_name()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.generators_read_kv(): {dss.generators_read_kv()}')
print(f'dss.generators_write_kv(): {dss.generators_write_kv(12.3)}')
print(f'dss.generators_read_kv(): {dss.generators_read_kv()}')
print(f'dss.generators_read_kw(): {dss.generators_read_kw()}')
print(f'dss.generators_write_kw(): {dss.generators_write_kw(15.8)}')
print(f'dss.generators_read_kw(): {dss.generators_read_kw()}')
print(f'dss.generators_read_kvar(): {dss.generators_read_kvar()}')
print(f'dss.generators_write_kvar(): {dss.generators_write_kvar(12.68)}')
print(f'dss.generators_read_kvar(): {dss.generators_read_kvar()}')
print(f'dss.generators_read_pf(): {dss.generators_read_pf()}')
print(f'dss.generators_write_pf(): {dss.generators_write_pf(15.32)}')
print(f'dss.generators_read_pf(): {dss.generators_read_pf()}')
print(f'dss.generators_read_kva_rated(): {dss.generators_read_kva_rated()}')
print(f'dss.generators_write_kva_rated(): {dss.generators_write_kva_rated(123)}')
print(f'dss.generators_read_kva_rated(): {dss.generators_read_kva_rated()}')
print(f'dss.generators_read_vmax_pu(): {dss.generators_read_vmax_pu()}')
print(f'dss.generators_write_vmax_pu(): {dss.generators_write_vmax_pu(32)}')
print(f'dss.generators_read_vmax_pu(): {dss.generators_read_vmax_pu()}')
print(f'dss.generators_read_vmin_pu(): {dss.generators_read_vmin_pu()}')
print(f'dss.generators_write_vmin_pu(): {dss.generators_write_vmin_pu(54)}')
print(f'dss.generators_read_vmin_pu(): {dss.generators_read_vmin_pu()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.generators_all_names(): {dss.generators_all_names()}')
print(f'dss.generators_register_names(): {dss.generators_register_names()}')
print(f'dss.generators_register_values(): {dss.generators_register_values()}')
