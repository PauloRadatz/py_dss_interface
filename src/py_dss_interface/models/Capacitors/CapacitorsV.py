# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CapacitorsV(Base):

    def capacitors_variant(self, first):
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CapacitorsV(ctypes.c_int32(first), variant_pointer)
        return variant_pointer.contents.value

    def capacitors_allnames(self):
        """Gets a variant array of strings with all Capacitor names in the circuit."""
        return self.capacitors_variant(0)

    def capacitors_read_states(self):
        """Gets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        return self.capacitors_variant(1)

    def capacitors_write_states(self, argument):
        """Sets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.CapacitorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
