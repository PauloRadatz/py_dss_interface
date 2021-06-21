# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.linecodes_count(): {dss.linecodes_count()}')
print(f'dss.linecodes_first(): {dss.linecodes_first()}')
print(f'dss.linecodes_next(): {dss.linecodes_next()}')
print(f'dss.linecodes_read_units(): {dss.linecodes_read_units()}')
print(f'dss.linecodes_write_units(): {dss.linecodes_write_units(5)}')
print(f'dss.linecodes_read_units(): {dss.linecodes_read_units()}')
print(f'dss.linecodes_read_phases(): {dss.linecodes_read_phases()}')
print(f'dss.linecodes_is_z1z0(): {dss.linecodes_is_z1z0()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.linecodes_read_name(): {dss.linecodes_read_name()}')
print(f'dss.linecodes_write_name(): {dss.linecodes_write_name("302")}')
print(f'dss.linecodes_read_name(): {dss.linecodes_read_name()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.linecodes_read_r1(): {dss.linecodes_read_r1()}')
print(f'dss.linecodes_write_r1(): {dss.linecodes_write_r1(12.2)}')
print(f'dss.linecodes_read_r1(): {dss.linecodes_read_r1()}')

print(f'dss.linecodes_read_x1(): {dss.linecodes_read_x1()}')
print(f'dss.linecodes_write_x1(): {dss.linecodes_write_x1(21.0)}')
print(f'dss.linecodes_read_x1(): {dss.linecodes_read_x1()}')

print(f'dss.linecodes_read_r0(): {dss.linecodes_read_r0()}')
print(f'dss.linecodes_write_r0(): {dss.linecodes_write_r0(22.1)}')
print(f'dss.linecodes_read_r0(): {dss.linecodes_read_r0()}')

print(f'dss.linecodes_read_x0(): {dss.linecodes_read_x0()}')
print(f'dss.linecodes_write_x0(): {dss.linecodes_write_x0(31.1)}')
print(f'dss.linecodes_read_x0(): {dss.linecodes_read_x0()}')

print(f'dss.linecodes_read_c1(): {dss.linecodes_read_c1()}')
print(f'dss.linecodes_write_c1(): {dss.linecodes_write_c1(13.1)}')
print(f'dss.linecodes_read_c1(): {dss.linecodes_read_c1()}')

print(f'dss.linecodes_read_c0(): {dss.linecodes_read_c0()}')
print(f'dss.linecodes_write_c0(): {dss.linecodes_write_c0(11.1)}')
print(f'dss.linecodes_read_c0(): {dss.linecodes_read_c0()}')

print(f'dss.linecodes_read_norm_amps(): {dss.linecodes_read_norm_amps()}')
print(f'dss.linecodes_write_norm_amps(): {dss.linecodes_write_norm_amps(700.2)}')
print(f'dss.linecodes_read_norm_amps(): {dss.linecodes_read_norm_amps()}')

print(f'dss.linecodes_read_emerg_amps(): {dss.linecodes_read_emerg_amps()}')
print(f'dss.linecodes_write_emerg_amps(): {dss.linecodes_write_emerg_amps(800.1)}')
print(f'dss.linecodes_read_emerg_amps(): {dss.linecodes_read_emerg_amps()}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.linecodes_read_rmatrix(): {dss.linecodes_read_rmatrix()}')
print(f'dss.linecodes_write_rmatrix(): {dss.linecodes_write_rmatrix("[0.791721 | 0.318476 | 0.781649 | 0.28345]")}')
print(f'dss.linecodes_read_rmatrix(): {dss.linecodes_read_rmatrix()}')
print(f'dss.linecodes_read_xmatrix(): {dss.linecodes_read_xmatrix()}')
print(f'dss.linecodes_write_xmatrix(): {dss.linecodes_write_xmatrix("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.linecodes_read_xmatrix(): {dss.linecodes_read_xmatrix()}')
print(f'dss.linecodes_read_cmatrix(): {dss.linecodes_read_cmatrix()}')
print(f'dss.linecodes_write_cmatrix(): {dss.linecodes_write_cmatrix("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.linecodes_read_cmatrix(): {dss.linecodes_read_cmatrix()}')
print(f'dss.linecodes_all_names(): {dss.linecodes_all_names()}')
