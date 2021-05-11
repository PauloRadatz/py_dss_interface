# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from typing import List

from comtypes import automation
from py_dss_interface.models.Base import Base


class LinesV(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are Variants.

    The structure of the interface is as follows:
        void LinesV(int32_t Parameter, , VARIANT *Argument);

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.

    """

    def lines_allnames(self) -> List[str]:
        """Gets the name of all Line Objects."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_rmatrix(self):
        """Gets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_rmatrix(self, argument) -> List[float]:
        """Sets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_xmatrix(self) -> List[float]:
        """Gets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_xmatrix(self, argument) -> List[float]:
        """Sets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_cmatrix(self) -> List[float]:
        """Gets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_cmatrix(self, argument) -> List[float]:
        """Sets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_yprim(self) -> List[float]:
        """Gets the YPrimitive of the active Line."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_yprim(self, argument) -> List[float]:
        """Does nothing at present."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value
