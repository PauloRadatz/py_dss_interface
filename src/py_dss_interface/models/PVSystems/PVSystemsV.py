# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class PVSystems(Base):

    # PVsystemsV (Variant)
    def pvsystems_allnames(self):
        """Gets the variant array of string containing names of all PVSystems in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.PVsystemsV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value
