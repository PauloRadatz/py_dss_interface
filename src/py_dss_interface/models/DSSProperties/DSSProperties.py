# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class DSSProperties(Base):
    """
    This interface implements the DSSproperties (IDSSProperties) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface.

    This interface can be used to read/write certain properties of DSS objects.

    The structure of the interface is as follows:
        CStr DSSProperties(int32_t Parameter, CStr Argument);

    This interface returns a string pointer (ANSI) with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """
    def dssproperties_name(self, argument: str) -> str:
        """Delivers the name of the active property. The index of the property must be specified in the argument.
        The index minimum value is 1. This value must be entered as string."""
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_description(self, argument: str) -> str:
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_read_value(self, argument: str) -> str:
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    # todo could you use this one?
    def dssproperties_write_value(self, argument: str) -> str:
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')
