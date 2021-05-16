# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 15/05/2021
"""
from py_dss_interface import DSS

dss = DSS()

dss_file = r"C:\eniocc\EPRI\py_dss_interface-master\src\py_dss_interface\tests\py_dss_interface\13Bus\IEEE13Nodeckt" \
           r".dss "

dss.text("compile {0}".format(dss_file))
