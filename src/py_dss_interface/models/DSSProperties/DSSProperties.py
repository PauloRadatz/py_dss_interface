# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class DSSProperties(Base):

    # DSSProperties
    def dssproperties_name(self, argument):
        """Delivers the name of the active property. The index of the property must be specified in the argument.
        The index minimum value is 1. This value must be entered as string."""
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_description(self, argument):
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_read_value(self, argument):
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_write_value(self, argument):
        result = ctypes.c_char_p(self.dss_obj.DSSProperties(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')
