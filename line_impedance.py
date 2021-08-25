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
dss = py_dss_interface.DSSDLL(r"C:\OpenDSS_svn\Version8\Source")

dss_file = r"C:\Users\ppra005\Box\Documents_PC\OpenDSS_forum\Jouni\line_impedances.dss"

dss.text("compile [{}]".format(dss_file))

dss.lines_write_name("line2")
dss.lines_read_r1()
