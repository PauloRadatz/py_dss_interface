# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.lines_first(): {dss.first()}')
print(f'dss.lines_next(): {dss.next()}')
print(f'dss.lines_read_phases(): {dss.phases_read()}')
print(f'dss.lines_write_phases(): {dss.phases_write(2)}')
print(f'dss.lines_read_phases(): {dss.phases_read()}')
print(f'dss.lines_num_cust(): {dss.num_cust()}')
print(f'dss.lines_parent(): {dss.parent()}')
print(f'dss.lines_count(): {dss.count()}')
print(f'dss.lines_read_units(): {dss.units_read()}')
print(f'dss.lines_write_units(): {dss.units_write(3)}')
print(f'dss.lines_read_units(): {dss.units_read()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.lines_read_name(): {dss.name_read()}')
print(f'dss.lines_write_name(): {dss.name_write("670671")}')
print(f'dss.lines_read_name(): {dss.name_read()}')
print(f'dss.lines_read_bus1(): {dss.bus1_read()}')
print(f'dss.lines_write_bus1(): {dss.bus1_write("670.1")}')
print(f'dss.lines_read_bus1(): {dss.bus1_read()}')
print(f'dss.lines_read_bus2(): {dss.bus2_read()}')
print(f'dss.lines_write_bus2(): {dss.bus2_write("671.2.3")}')
print(f'dss.lines_read_bus2(): {dss.bus2_read()}')
print(f'dss.lines_read_linecode(): {dss.linecode_read()}')
print(f'dss.lines_write_linecode(): {dss.linecode_write("723")}')
print(f'dss.lines_read_linecode(): {dss.linecode_read()}')
print(f'dss.lines_read_geometry(): {dss.geometry_read()}')
print(f'dss.lines_write_geometry(): {dss.geometry_write("")}')
print(f'dss.lines_read_geometry(): {dss.geometry_read()}')
print(f'dss.lines_read_spacing(): {dss.spacing_read()}')
print(f'dss.lines_write_spacing(): {dss.spacing_write("")}')
print(f'dss.lines_read_spacing(): {dss.spacing_read()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.lines_read_length(): {dss.length_read()}')
print(f'dss.lines_write_length(): {dss.length_write(152.1)}')
print(f'dss.lines_read_length(): {dss.length_read()}')

print(f'dss.lines_read_r1(): {dss.r1_read()}')
print(f'dss.lines_write_r1(): {dss.r1_write(1.2)}')
print(f'dss.lines_read_r1(): {dss.r1_read()}')

print(f'dss.lines_read_x1(): {dss.x1_read()}')
print(f'dss.lines_write_x1(): {dss.x1_write(1.23)}')
print(f'dss.lines_read_x1(): {dss.x1_read()}')

print(f'dss.lines_read_r0(): {dss.r0_read()}')
print(f'dss.lines_write_r0(): {dss.r0_write(1.2)}')
print(f'dss.lines_read_r0(): {dss.r0_read()}')

print(f'dss.lines_read_x0(): {dss.x0_read()}')
print(f'dss.lines_write_x0(): {dss.x0_write(1.2)}')
print(f'dss.lines_read_x0(): {dss.x0_read()}')

print(f'dss.lines_read_c1(): {dss.c1_read()}')
print(f'dss.lines_write_c1(): {dss.c1_write(2.1)}')
print(f'dss.lines_read_c1(): {dss.c1_read()}')

print(f'dss.lines_read_c0(): {dss.c0_read()}')
print(f'dss.lines_write_c0(): {dss.c0_write(1.32)}')
print(f'dss.lines_read_c0(): {dss.c0_read()}')

print(f'dss.lines_read_norm_amps(): {dss.norm_amps_read()}')
print(f'dss.lines_write_norm_amps(): {dss.norm_amps_write(123)}')
print(f'dss.lines_read_norm_amps(): {dss.norm_amps_read()}')

print(f'dss.lines_read_emerg_amps(): {dss.emerg_amps_read()}')
print(f'dss.lines_write_emerg_amps(): {dss.emerg_amps_write(321)}')
print(f'dss.lines_read_emerg_amps(): {dss.emerg_amps_read()}')

print(f'dss.lines_read_rg(): {dss.rg_read()}')
print(f'dss.lines_write_rg(): {dss.rg_write(111)}')
print(f'dss.lines_read_rg(): {dss.rg_read()}')

print(f'dss.lines_read_xg(): {dss.xg_read()}')
print(f'dss.lines_write_xg(): {dss.xg_write(12)}')
print(f'dss.lines_read_xg(): {dss.xg_read()}')

print(f'dss.lines_read_rho(): {dss.rho_read()}')
print(f'dss.lines_write_rho(): {dss.rho_write(111)}')
print(f'dss.lines_read_rho(): {dss.rho_read()}')

print(f'dss.lines_read_season_rating(): {dss.season_rating_read()}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.lines_all_names(): {dss.names()}')
print(f'dss.lines_read_rmatrix(): {dss.rmatrix_read()}')
print(f'dss.lines_write_rmatrix(): {dss.rmatrix_write("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_rmatrix(): {dss.rmatrix_read()}')
print(f'dss.lines_read_xmatrix(): {dss.xmatrix_read()}')
print(f'dss.lines_write_xmatrix(): {dss.xmatrix_write("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_xmatrix(): {dss.xmatrix_read()}')
print(f'dss.lines_read_cmatrix(): {dss.cmatrix_read()}')
print(f'dss.lines_write_cmatrix(): {dss.cmatrix_write("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_cmatrix(): {dss.cmatrix_read()}')
print(f'dss.lines_read_yprim(): {dss.yprim_read()}')
print(f'dss.lines_write_yprim(): {dss.yprim_write("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_yprim(): {dss.yprim_read()}')
