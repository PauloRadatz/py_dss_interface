# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class PDElements(Base):

    # PDElementsS (String)
    def pdelements_read_name(self):
        """Gets the name of the active PDElement, returns null string if active element id not PDElement."""
        result = ctypes.c_char_p(self.dss_obj.PDElementsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def pdelements_write_name(self, argument):
        """Sets the name of the active PDElement, returns null string if active element id not PDElement."""
        result = ctypes.c_char_p(self.dss_obj.PDElementsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')
