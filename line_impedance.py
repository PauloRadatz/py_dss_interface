# -*- coding: utf-8 -*-
# @Time    : 8/11/2021 5:22 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : line_impedance.py
# @Software: PyCharm


import py_dss_interface
import pathlib
import os
# Creates an OpenDSS object
dss = py_dss_interface.DSS(r"C:\OpenDSS_svn\Version8\Source")

dss_file = r"C:\Users\ppra005\Box\Documents_PC\OpenDSS_forum\Jouni\line_impedances.dss"

dss.text(f"compile [{dss_file}]")

dss.name_write("line2")
dss.r1_read()
