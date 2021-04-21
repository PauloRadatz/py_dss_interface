# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class DSSElement(Base):

    # DSSElementI (int)
    def dsselement_numproperties(self):
        """Gets the number of properties for the active DSS object."""
        result = int(self.dss_obj.DSSElementI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    # DSSElementS (String)
    def dsselement_name(self):
        """Gets the full name of the active DSS object (general element or circuit element)."""
        result = ctypes.c_char_p(self.dss_obj.DSSElementS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # DSSElementV (Variant)
    def dsselement_allpropertynames(self):
        """Gets a variant array of strings containing the names of all properties for the active DSS object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSElementV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
