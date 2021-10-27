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

    def lines_read_name(self) -> str:
        """Gets the name of the active Line element."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_name(self, argument: str) -> str:
        """Sets the name of the Line element to set it active."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_bus1(self) -> str:
        """Gets the name of bus for terminal 1."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_bus1(self, argument: str) -> str:
        """Sets the name of bus for terminal 1."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_bus2(self) -> str:
        """Gets the name of bus for terminal 2."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_bus2(self, argument: str) -> str:
        """Sets the name of bus for terminal 2."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_linecode(self) -> str:
        """Gets the name of LineCode object that defines the impedances."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_linecode(self, argument: str) -> str:
        """Sets the name of LineCode object that defines the impedances."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_geometry(self) -> str:
        """Gets the name of the Line geometry code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_geometry(self, argument: str) -> str:
        """Sets the name of the Line geometry code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_spacing(self) -> str:
        """Gets the name of the Line spacing code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_spacing(self, argument: str) -> str:
        """Sets the name of the Line spacing code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')
