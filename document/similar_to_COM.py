# -*- coding: utf-8 -*-
# @Time    : 7/11/2023 4:11 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : similar_to_COM.py
# @Software: PyCharm

import ctypes

dll_path = r"C:\Program Files\OpenDSS\x64\OpenDSSDirect.dll"

DSSObject = ctypes.cdll.LoadLibrary(dll_path)

if not int(DSSObject.DSSI(ctypes.c_int32(3), ctypes.c_int32(0))):
    print('Unable to start openDSS')
    exit()
else:
    print("Able to start openDSS")
