# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class SolutionS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr SolutionS(int32_t Parameter, CStr Argument);

    This interface returns a string according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _mode_id(self):
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _ld_curve_read(self):
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _ld_curve_write(self, argument):
        argument = Base._check_string_param(argument)
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _default_daily_read(self):
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _default_daily_write(self, argument):
        argument = Base._check_string_param(argument)
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _default_yearly_read(self):
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _default_yearly_write(self, argument):
        argument = Base._check_string_param(argument)
        result = ctypes.c_char_p(self._dss_obj.SolutionS(ctypes.c_int32(6), argument.encode('ascii')))
        return result.value.decode('ascii')
