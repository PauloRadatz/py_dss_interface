# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class FusesI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t FusesI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def _count(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _first(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _next(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _monitored_term(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(3), ctypes.c_int32(0))

    def _monitored_term_write(self, argument: int) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def _switched_term(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _switched_term_write(self, argument: int) -> int:
        argument = Base._check_int_param(argument)
        return self._dss_obj.FusesI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def _open(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _close(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(8), ctypes.c_int32(0))

    def _is_blown(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _idx(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _idx_write(self, argument: int) -> int:
        argument = Base._check_int_param(argument, default=1)
        return self._dss_obj.FusesI(ctypes.c_int32(11), ctypes.c_int32(argument))

    def _num_phases(self) -> int:
        return self._dss_obj.FusesI(ctypes.c_int32(12), ctypes.c_int32(0))

    def _reset(self):
        return self._dss_obj.FusesI(ctypes.c_int32(13), ctypes.c_int32(0))
