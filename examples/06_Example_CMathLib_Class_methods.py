# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 12/05/2021
"""
import os
import pathlib
from py_dss_interface import DSSDLL as DSS

dss = DSS()

my_path = pathlib.Path(__file__).parents[1] # one level above
my_path = os.path.join(my_path, r"test\py_dss_interface\13Bus")
dss_file = os.path.join(my_path, "IEEE13Nodeckt.dss")

dss.text("compile {0}".format(dss_file))

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')


# To iterate from begin we must call first()
# dss.active_class_first()
# for i in range(dss.active_class_num_elements()):
#     print(f'Name: {dss.active_class_get_name()} || Index: {i}')
#     dss.active_class_next()


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
