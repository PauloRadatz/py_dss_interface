# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class PVSystems(Base):

    # PVsystemsS (String)
    def pvsystems_read_name(self):
        """Gets the name of the active PVSystem."""
        result = ctypes.c_char_p(self.dss_obj.PVsystemsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def pvsystems_write_name(self, argument):
        """Sets the name of the active PVSystem."""
        result = ctypes.c_char_p(self.dss_obj.PVsystemsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # PVsystemsV (Variant)
    def pvsystems_allnames(self):
        """Gets the variant array of string containing names of all PVSystems in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.PVsystemsV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value
