# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class PVSystemsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t PVSystemsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def _count(self) -> int:
        return int(self._dss_obj.PVsystemsI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def _first(self) -> int:
        return int(self._dss_obj.PVsystemsI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def _next(self) -> int:
        return int(self._dss_obj.PVsystemsI(ctypes.c_int32(2), ctypes.c_int32(0)))

    def _idx_read(self) -> int:
        return int(self._dss_obj.PVsystemsI(ctypes.c_int32(3), ctypes.c_int32(0)))

    def _idx_write(self, argument: int) -> int:
        return int(self._dss_obj.PVsystemsI(ctypes.c_int32(4), ctypes.c_int32(argument)))
