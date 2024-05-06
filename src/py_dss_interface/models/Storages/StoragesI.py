# -*- coding: utf-8 -*-
# @Time    : 5/6/2024 7:32 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : StoragesI.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models.Base import Base


class StoragesI(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are integers.

    The structure of the interface is as follows:
        int32_t StoragesI(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _first(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _next(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _count(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _idx_read(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(3), ctypes.c_int32(0))

    def _idx_write(self, argument: int) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def _state_read(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _state_write(self, argument: int) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def _control_mode_read(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _control_mode_write(self, argument: int) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(8), ctypes.c_int32(argument))

    def _safe_mode(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _var_follow_inverter_read(self) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _var_follow_inverter_write(self, argument: int) -> int:
        return self._dss_obj.StoragesI(ctypes.c_int32(11), ctypes.c_int32(argument))
