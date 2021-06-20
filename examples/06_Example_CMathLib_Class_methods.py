# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')


# Float methods
print(45 * '=' + ' Float Methods ' + 45 * '=')
print(f'dss.cmathlib_cabs: {dss.cmathlib_cabs()}')
print(f'dss.cmathlib_cdang(): {dss.cmathlib_cdang()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')


# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
# TODO: Ênio - we need validate these methods below
print(f'dss.cmathlib_cmplx(): {dss.cmathlib_cmplx(1.3, 1.8)}')
print(f'dss.cmathlib_ctopolardeg(): {dss.cmathlib_ctopolardeg(2, 3)}')
print(f'dss.cmathlib_pdegtocomplex(): {dss.cmathlib_pdegtocomplex(3.605551275463989, 0.982793723247329)}')
