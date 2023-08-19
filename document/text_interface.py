# -*- coding: utf-8 -*-
# @Time    : 7/11/2023 4:22 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : text_interface.py
# @Software: PyCharm

import ctypes

dll_path = r"C:\Program Files\OpenDSS\x64\OpenDSSDirect.dll"

DSSObject = ctypes.cdll.LoadLibrary(dll_path)

dss_file = r"C:\Program Files\OpenDSS\IEEETestCases\13Bus\IEEE13Nodeckt.dss"
argument = f"compile [{dss_file}]"
# This below can be a function
result = ctypes.c_char_p(DSSObject.DSSPut_Command(argument.encode('ascii')))

argument = "solve"
result = ctypes.c_char_p(DSSObject.DSSPut_Command(argument.encode('ascii')))

argument = "show voltages ln nodes"
result = ctypes.c_char_p(DSSObject.DSSPut_Command(argument.encode('ascii')))
