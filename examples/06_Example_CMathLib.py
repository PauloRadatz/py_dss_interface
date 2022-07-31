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
print(f'dss.cmathlib_cabs: {dss.cabs(3, -4)}')  # absolute from 3 -4j is 5
print(f'dss.cmathlib_cdang(): {dss.cdang(1, 1)}')  # degres from 1+1j is 45


# Variant methods
print(45 * '=' + ' Variant Methods ' + 45 * '=')
print(f'dss.cmathlib_cmplx(): {dss.cmplx(1.3, 1.8)}')  # must return 1.3 + 1.8j
print(f'dss.cmathlib_ctopolardeg(): {dss.ctopolardeg(1, 3)}')  # must return 3.1622776601683795,
# 1.2490457723982544
print(f'dss.cmathlib_pdegtocomplex(): {dss.pdegtocomplex(3.1622776601683795, 1.2490457723982544)}')  # must
# return 1+3j
