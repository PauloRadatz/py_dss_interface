# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.linecodes_count(): {dss._count()}')
print(f'dss.linecodes_first(): {dss.first()}')
print(f'dss.linecodes_next(): {dss.next()}')
print(f'dss.linecodes_read_units(): {dss.units_read()}')
print(f'dss.linecodes_write_units(): {dss.units_write(5)}')
print(f'dss.linecodes_read_units(): {dss.units_read()}')
print(f'dss.linecodes_read_phases(): {dss.phases_read()}')
print(f'dss.linecodes_is_z1z0(): {dss.is_z1z0()}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
print(f'dss.linecodes_read_name(): {dss.name_read()}')
print(f'dss.linecodes_write_name(): {dss.name_write("302")}')
print(f'dss.linecodes_read_name(): {dss.name_read()}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
print(f'dss.linecodes_read_r1(): {dss.r1_read()}')
print(f'dss.linecodes_write_r1(): {dss.r1_write(12.2)}')
print(f'dss.linecodes_read_r1(): {dss.r1_read()}')

print(f'dss.linecodes_read_x1(): {dss.x1_read()}')
print(f'dss.linecodes_write_x1(): {dss.x1_write(21.0)}')
print(f'dss.linecodes_read_x1(): {dss.x1_read()}')

print(f'dss.linecodes_read_r0(): {dss.r0_read()}')
print(f'dss.linecodes_write_r0(): {dss.r0_write(22.1)}')
print(f'dss.linecodes_read_r0(): {dss.r0_read()}')

print(f'dss.linecodes_read_x0(): {dss.x0_read()}')
print(f'dss.linecodes_write_x0(): {dss.x0_write(31.1)}')
print(f'dss.linecodes_read_x0(): {dss.x0_read()}')

print(f'dss.linecodes_read_c1(): {dss.c1_read()}')
print(f'dss.linecodes_write_c1(): {dss.c1_write(13.1)}')
print(f'dss.linecodes_read_c1(): {dss.c1_read()}')

print(f'dss.linecodes_read_c0(): {dss.c0_read()}')
print(f'dss.linecodes_write_c0(): {dss.c0_write(11.1)}')
print(f'dss.linecodes_read_c0(): {dss.c0_read()}')

print(f'dss.linecodes_read_norm_amps(): {dss.norm_amps_read()}')
print(f'dss.linecodes_write_norm_amps(): {dss.norm_amps_write(700.2)}')
print(f'dss.linecodes_read_norm_amps(): {dss.norm_amps_read()}')

print(f'dss.linecodes_read_emerg_amps(): {dss.emerg_amps_read()}')
print(f'dss.linecodes_write_emerg_amps(): {dss.emerg_amps_write(800.1)}')
print(f'dss.linecodes_read_emerg_amps(): {dss.emerg_amps_read()}')


# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.linecodes_read_rmatrix(): {dss.rmatrix_read()}')
print(f'dss.linecodes_write_rmatrix(): {dss.rmatrix_write("[0.791721 | 0.318476 | 0.781649 | 0.28345]")}')
print(f'dss.linecodes_read_rmatrix(): {dss.rmatrix_read()}')
print(f'dss.linecodes_read_xmatrix(): {dss.xmatrix_read()}')
print(f'dss.linecodes_write_xmatrix(): {dss.xmatrix_write("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.linecodes_read_xmatrix(): {dss.xmatrix_read()}')
print(f'dss.linecodes_read_cmatrix(): {dss.cmatrix_read()}')
print(f'dss.linecodes_write_cmatrix(): {dss.cmatrix_write("[1.3569 | 0.4591 1.3471]")}')
print(f'dss.linecodes_read_cmatrix(): {dss.cmatrix_read()}')
print(f'dss.linecodes_all_names(): {dss.names()}')
