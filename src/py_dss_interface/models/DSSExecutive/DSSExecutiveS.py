# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSExecutiveS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr DSSExecutiveS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _command(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSExecutiveS(ctypes.c_int32(0), arg.encode('ascii')))
        return result.value.decode('ascii')

    def _option(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSExecutiveS(ctypes.c_int32(1), arg.encode('ascii')))
        return result.value.decode('ascii')

    def _command_help(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSExecutiveS(ctypes.c_int32(2), arg.encode('ascii')))
        return result.value.decode('ascii')

    def _option_help(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSExecutiveS(ctypes.c_int32(3), arg.encode('ascii')))
        return result.value.decode('ascii')

    def _option_value(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSExecutiveS(ctypes.c_int32(4), arg.encode('ascii')))
        return result.value.decode('ascii')
