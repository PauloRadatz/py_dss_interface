# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MetersS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr MetersS(int32_t Parameter, CStr Argument);

    This interface returns a string according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def meters_read_name(self):
        """Returns the active Energy Meter's name."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def meters_write_name(self, argument):
        """Sets the active Energy Meter's name."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def meters_read_metered_element(self):
        """Returns the name of the metered element (considering the active Energy Meter)."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def meters_write_metered_element(self, argument):
        """Sets the name of the metered element (considering the active Energy Meter)."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')
