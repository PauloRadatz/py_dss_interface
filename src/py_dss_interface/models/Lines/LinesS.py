# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LinesS(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are Strings.

    The structure of the interface is as follows:
        CStr LinesS(int32_t Parameter, CStr argument)

    This interface returns a string, the variable “parameter” is used to specify the property of the class to be used
    and the variable “argument” can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _name_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _name_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _bus1_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _bus1_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _bus2_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _bus2_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # TODO: check conflict with another method in this class. Are they the same?
    def _linecode_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TODO: check conflict with another method in this class. Are they the same?
    def _linecode_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _geometry_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _geometry_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _spacing_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _spacing_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.LinesS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')
