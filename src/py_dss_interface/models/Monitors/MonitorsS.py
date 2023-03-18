# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MonitorsS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr MonitorsS(int32_t Parameter, CStr Argument);

    This interface returns a string according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _file_name(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.MonitorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _name_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.MonitorsS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _name_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.MonitorsS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _element_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.MonitorsS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _element_write(self, argument) -> str:
        result = ctypes.c_char_p(self._dss_obj.MonitorsS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')
