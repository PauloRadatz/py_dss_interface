# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class LineCodesV(Base):
    """
    This interface can be used to read/modify the properties of the LineCode Class where the values are Variants.

    The structure of the interface is as follows:
        void LineCodesV(int32_t Parameter, , VARIANT *Argument);

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """
    def linecodes_read_rmatrix(self):
        """Gets the resistance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_rmatrix(self, argument):
        """Sets the resistance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LineCodesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_read_xmatrix(self):
        """Gets the reactance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_xmatrix(self, argument):
        """Sets the reactance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LineCodesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_read_cmatrix(self):
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_cmatrix(self, argument):
        """Sets the capacitance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LineCodesV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_allnames(self):
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value
