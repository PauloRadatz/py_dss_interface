# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com
# @File    : DSSPropertiesS.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models.Base import Base


class DSSPropertiesS(Base):
    """
    This interface implements the DSSproperties (IDSSProperties) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface.

    This interface can be used to read/write certain properties of DSS objects.

    The structure of the interface is as follows:
        CStr DSSProperties(int32_t Parameter, CStr Argument);

    This interface returns a string pointer (ANSI) with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def _name_active_property(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSProperties(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _description_active_property(self, argument: str) -> str:
        to_int = int(argument)
        if to_int < 1:
            return "ERROR: The value must be greater than 1!"
        result = ctypes.c_char_p(self._dss_obj.DSSProperties(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _value_read(self, argument: str) -> str:

        to_int = int(argument)
        if to_int < 1:
            return "ERROR: The value must be greater than 1!"
        result = ctypes.c_char_p(self._dss_obj.DSSProperties(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    # TODO include in test
    def _value_write(self, argument: str) -> str:
        to_int = int(argument)
        if to_int < 1:
            return "ERROR: The value must be greater than 1!"
        result = ctypes.c_char_p(self._dss_obj.DSSProperties(ctypes.c_int32(3), argument.encode('ascii')))
        result = result.value.decode('ascii')
        if result == '':
            print("Value written successfully!")
        return result
