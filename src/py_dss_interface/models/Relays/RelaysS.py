# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class RelaysS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr RelaysS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _name_read(self):
        result = ctypes.c_char_p(self._dss_obj.RelaysS(ctypes.c_int32(0), "".encode('ascii')))
        return result.value.decode('ascii')

    def _name_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.RelaysS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _monitored_obj_read(self):
        result = ctypes.c_char_p(self._dss_obj.RelaysS(ctypes.c_int32(2), "".encode('ascii')))
        return result.value.decode('ascii')

    def _monitored_obj_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.RelaysS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _switched_obj_read(self):
        result = ctypes.c_char_p(self._dss_obj.RelaysS(ctypes.c_int32(4), "".encode('ascii')))
        return result.value.decode('ascii')

    def _switched_obj_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.RelaysS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

