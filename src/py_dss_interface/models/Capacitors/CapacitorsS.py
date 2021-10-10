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

    def capacitors_read_name(self) -> str:
        """Gets the name of the active Capacitor element."""
        return (self.dss_obj.CapacitorsS(0, 0)).decode('ascii')

    def capacitors_write_name(self, capacitor_name: str) -> str:
        """Sets the name of the Capacitor element to set it active. There is not a explicit return type in the
        oficial documentation, because of this we choose not put a explicit return too.
        :param capacitor_name: the intended name to the capacitor
        """
        capacitor_name = Base.check_string_param(capacitor_name)
        ctypes.c_char_p(capacitor_name.encode('utf-8'))
        return (self.dss_obj.CapacitorsS(1, ctypes.c_char_p(capacitor_name.encode('utf-8')))).decode('ascii')
