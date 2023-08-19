# -*- coding: utf-8 -*-
# @Time    : 7/11/2023 4:28 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : text_interface_PR.py
# @Software: PyCharm

import py_dss_interface

dss = py_dss_interface.DSS()

dss_file = r"C:\Program Files\OpenDSS\IEEETestCases\13Bus\IEEE13Nodeckt.dss"

dss.text(f"compile [{dss_file}]")
dss.text("solve")
dss.text("show voltages ln nodes")
