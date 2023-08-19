# -*- coding: utf-8 -*-
# @Time    : 7/11/2023 4:26 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : similar_to_com_PR.py
# @Software: PyCharm

import py_dss_interface

dss = py_dss_interface.DSS()

if not dss.dssinterface.start():
    print('Unable to start openDSS')
    exit()
else:
    print("Able to start openDSS")
