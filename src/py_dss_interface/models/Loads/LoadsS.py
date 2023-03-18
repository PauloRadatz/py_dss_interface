# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models.Base import Base


class LoadsS(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are strings.

    The structure of the interface is as follows:
        CStr DSSLoadsS(int32_t Parameter, CStr Argument);

    This interface returns a string, the variable “parameter” (Integer) is used to specify the property of the class
    to be used and the variable “argument” (string) can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.
    """

    def _name(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _name_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _cvr_curve(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _cvr_curve_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _daily(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _daily_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _duty(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TODO include in test
    def _duty_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _spectrum(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _spectrum_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _yearly(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _yearly_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _growth(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(12), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _growth_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSLoadsS(ctypes.c_int32(13), argument.encode('ascii')))
        return result.value.decode('ascii')
