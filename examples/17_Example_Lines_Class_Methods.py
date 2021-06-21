# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.lines_first(): {dss.lines_first()}')
print(f'dss.lines_next(): {dss.lines_next()}')
print(f'dss.lines_read_phases(): {dss.lines_read_phases()}')
print(f'dss.lines_write_phases(): {dss.lines_write_phases(2)}')
print(f'dss.lines_read_phases(): {dss.lines_read_phases()}')
print(f'dss.lines_num_cust(): {dss.lines_num_cust()}')
print(f'dss.lines_parent(): {dss.lines_parent()}')
print(f'dss.lines_count(): {dss.lines_count()}')
print(f'dss.lines_read_units(): {dss.lines_read_units()}')
print(f'dss.lines_write_units(): {dss.lines_write_units(3)}')
print(f'dss.lines_read_units(): {dss.lines_read_units()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.lines_read_name(): {dss.lines_read_name()}')
print(f'dss.lines_write_name(): {dss.lines_write_name("670671")}')
print(f'dss.lines_read_name(): {dss.lines_read_name()}')
print(f'dss.lines_read_bus1(): {dss.lines_read_bus1()}')
print(f'dss.lines_write_bus1(): {dss.lines_write_bus1("670.1")}')
print(f'dss.lines_read_bus1(): {dss.lines_read_bus1()}')
print(f'dss.lines_read_bus2(): {dss.lines_read_bus2()}')
print(f'dss.lines_write_bus2(): {dss.lines_write_bus2("671.2.3")}')
print(f'dss.lines_read_bus2(): {dss.lines_read_bus2()}')
print(f'dss.lines_read_linecode(): {dss.lines_read_linecode()}')
print(f'dss.lines_write_linecode(): {dss.lines_write_linecode("723")}')
print(f'dss.lines_read_linecode(): {dss.lines_read_linecode()}')
print(f'dss.lines_read_geometry(): {dss.lines_read_geometry()}')
print(f'dss.lines_write_geometry(): {dss.lines_write_geometry("")}')
print(f'dss.lines_read_geometry(): {dss.lines_read_geometry()}')
print(f'dss.lines_read_spacing(): {dss.lines_read_spacing()}')
print(f'dss.lines_write_spacing(): {dss.lines_write_spacing("")}')
print(f'dss.lines_read_spacing(): {dss.lines_read_spacing()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.lines_read_length(): {dss.lines_read_length()}')
print(f'dss.lines_write_length(): {dss.lines_write_length(152.1)}')
print(f'dss.lines_read_length(): {dss.lines_read_length()}')

print(f'dss.lines_read_r1(): {dss.lines_read_r1()}')
print(f'dss.lines_write_r1(): {dss.lines_write_r1(1.2)}')
print(f'dss.lines_read_r1(): {dss.lines_read_r1()}')

print(f'dss.lines_read_x1(): {dss.lines_read_x1()}')
print(f'dss.lines_write_x1(): {dss.lines_write_x1(1.23)}')
print(f'dss.lines_read_x1(): {dss.lines_read_x1()}')

print(f'dss.lines_read_r0(): {dss.lines_read_r0()}')
print(f'dss.lines_write_r0(): {dss.lines_write_r0(1.2)}')
print(f'dss.lines_read_r0(): {dss.lines_read_r0()}')

print(f'dss.lines_read_x0(): {dss.lines_read_x0()}')
print(f'dss.lines_write_x0(): {dss.lines_write_x0(1.2)}')
print(f'dss.lines_read_x0(): {dss.lines_read_x0()}')

print(f'dss.lines_read_c1(): {dss.lines_read_c1()}')
print(f'dss.lines_write_c1(): {dss.lines_write_c1(2.1)}')
print(f'dss.lines_read_c1(): {dss.lines_read_c1()}')

print(f'dss.lines_read_c0(): {dss.lines_read_c0()}')
print(f'dss.lines_write_c0(): {dss.lines_write_c0(1.32)}')
print(f'dss.lines_read_c0(): {dss.lines_read_c0()}')

print(f'dss.lines_read_norm_amps(): {dss.lines_read_norm_amps()}')
print(f'dss.lines_write_norm_amps(): {dss.lines_write_norm_amps(123)}')
print(f'dss.lines_read_norm_amps(): {dss.lines_read_norm_amps()}')

print(f'dss.lines_read_emerg_amps(): {dss.lines_read_emerg_amps()}')
print(f'dss.lines_write_emerg_amps(): {dss.lines_write_emerg_amps(321)}')
print(f'dss.lines_read_emerg_amps(): {dss.lines_read_emerg_amps()}')

print(f'dss.lines_read_rg(): {dss.lines_read_rg()}')
print(f'dss.lines_write_rg(): {dss.lines_write_rg(111)}')
print(f'dss.lines_read_rg(): {dss.lines_read_rg()}')

print(f'dss.lines_read_xg(): {dss.lines_read_xg()}')
print(f'dss.lines_write_xg(): {dss.lines_write_xg(12)}')
print(f'dss.lines_read_xg(): {dss.lines_read_xg()}')

print(f'dss.lines_read_rho(): {dss.lines_read_rho()}')
print(f'dss.lines_write_rho(): {dss.lines_write_rho(111)}')
print(f'dss.lines_read_rho(): {dss.lines_read_rho()}')

print(f'dss.lines_read_season_rating(): {dss.lines_read_season_rating()}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.lines_all_names(): {dss.lines_all_names()}')
print(f'dss.lines_read_rmatrix(): {dss.lines_read_rmatrix()}')
print(f'dss.lines_write_rmatrix(): {dss.lines_write_rmatrix("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_rmatrix(): {dss.lines_read_rmatrix()}')
print(f'dss.lines_read_xmatrix(): {dss.lines_read_xmatrix()}')
print(f'dss.lines_write_xmatrix(): {dss.lines_write_xmatrix("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_xmatrix(): {dss.lines_read_xmatrix()}')
print(f'dss.lines_read_cmatrix(): {dss.lines_read_cmatrix()}')
print(f'dss.lines_write_cmatrix(): {dss.lines_write_cmatrix("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_cmatrix(): {dss.lines_read_cmatrix()}')
print(f'dss.lines_read_yprim(): {dss.lines_read_yprim()}')
print(f'dss.lines_write_yprim(): {dss.lines_write_yprim("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.lines_read_yprim(): {dss.lines_read_yprim()}')
