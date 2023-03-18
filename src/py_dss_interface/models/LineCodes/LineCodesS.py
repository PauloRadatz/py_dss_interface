# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LineCodesS(Base):
    """
    This interface can be used to read/modify the properties of the LineCode Class where the values are Strings.

    The structure of the interface is as follows:
        CStr LineCodesS(int32_t Parameter, CStr argument)

    This interface returns a string, the variable “parameter” is used to specify the property of the class to be used
    and the variable “argument” can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.;
    """

    def _name_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.LineCodesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _name_write(self, argument: str) -> str:
        argument = Base._check_string_param(argument)
        result = ctypes.c_char_p(self._dss_obj.LineCodesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')
