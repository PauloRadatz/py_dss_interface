# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CapacitorsS(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are Strings.

    The structure of the interface is as follows:
        CStr CapacitorsS(int32_t Parameter, CStr capacitor_name)

    This interface returns a string, the first parameter is used to specify the property of the class to be used and
    the second parameter can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _name(self) -> str:
        return (self._dss_obj.CapacitorsS(0, 0)).decode('ascii')

    def _name_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.CapacitorsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')
