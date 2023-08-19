# -*- coding: utf-8 -*-
# @Time    : 7/11/2023 4:20 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : circuit_interface.py
# @Software: PyCharm

import ctypes

dll_path = r"C:\Program Files\OpenDSS\x64\OpenDSSDirect.dll"

DSSObject = ctypes.cdll.LoadLibrary(dll_path)

DSSObject.CircuitF.restype = ctypes.c_double
DSSObject.CircuitS.restype = ctypes.c_char_p

dss_file = r"C:\Program Files\OpenDSS\IEEETestCases\13Bus\IEEE13Nodeckt.dss"
argument = f"compile [{dss_file}]"
result = ctypes.c_char_p(DSSObject.DSSPut_Command(argument.encode('ascii')))
print(result.value.decode("ascii"))

argument = "new energymeter.m element=Transformer.Sub"
result = ctypes.c_char_p(DSSObject.DSSPut_Command(argument.encode('ascii')))

argument = "solve"
result = ctypes.c_char_p(DSSObject.DSSPut_Command(argument.encode('ascii')))

# Int
num_nodes = DSSObject.CircuitI(ctypes.c_int32(2), ctypes.c_int32(0))
print(num_nodes)

# Float
capacity = DSSObject.CircuitF(ctypes.c_int32(0), ctypes.c_double(0), ctypes.c_double(0.1))
print(capacity)

# String
circuit_name = ctypes.c_char_p(DSSObject.CircuitS(ctypes.c_int32(0), ctypes.c_int32(0))).value.decode('ascii')
print(circuit_name)

argument = "Transformer.Sub"
cktelement_active = ctypes.c_char_p(DSSObject.CircuitS(ctypes.c_int32(3), argument.encode('ascii'))).value.decode('ascii')
print(cktelement_active)


# Variant (PR: I assume we need to use the new version, if it is the case we can add it here later).
