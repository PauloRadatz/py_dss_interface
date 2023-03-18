# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class FusesS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr FusesS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _name(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _name_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _monitored_obj(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _monitored_obj_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _switched_obj(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _switched_obj_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _tcc_curve(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _tcc_curve_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.FusesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')
