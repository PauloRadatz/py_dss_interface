# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class LoadsV(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are variants (the
    value can have different formats).

    The structure of the interface is as follows:
        void DSSLoadsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a string, the variable “parameter” (Integer) is used to specify the property of the class
    to be used and the variable “argument” (Variant) is used to return the variant structure.
    """

    def loads_allnames(self):
        """Allows to read the names of all the loads present in the active circuit. The result is delivered as
        variant, however, the content of this variant is an array of strings. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSLoadsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def loads_read_zipv(self):
        """Allows to read the array of 7 elements (doubles) for ZIP property of the active Load object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSLoadsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def loads_write_zipv(self, argument):
        """Allows to write the array of 7 elements (doubles) for ZIP property of the active Load object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.DSSLoadsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
