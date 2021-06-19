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

    def solution_mode_id(self):
        """Returns the ID (text) of the present solution mode."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_read_ld_curve(self):
        """Returns the Load-Duration Curve name for LD modes."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_ld_curve(self, argument):
        """Sets the Load-Duration Curve name for LD modes."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def solution_read_default_daily(self):
        """Returns the default daily load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_default_daily(self, argument):
        """Sets the default daily load shape (defaults to "Default")."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def solution_read_default_yearly(self):
        """Returns the default yearly load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_default_yearly(self, argument):
        """Sets the default yearly load shape (defaults to "Default")."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(6), argument.encode('ascii')))
        return result.value.decode('ascii')
