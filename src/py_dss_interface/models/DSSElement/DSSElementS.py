# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSElementS(Base):
    """
    This interface can be used to read/modify the properties of the DSSElement Class where the values are Strings.

    The structure of the interface is as follows:
        CStr DSSElementS(int32_t Parameter, CStr argument) ;

    This interface returns a string, the variable “parameter” is used to specify the property of the class to be used
    and the variable “argument” can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _name(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSElementS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')
