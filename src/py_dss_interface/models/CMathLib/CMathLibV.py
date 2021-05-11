# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from typing import List

from comtypes import automation
from py_dss_interface.models.Base import Base


class CMathLibV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CmathLibV(int32_t Parameter, double Argument1, Argument2, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def cmathlib_cmplx(self):
        """Convert real (Argument1) and imaginary (Argument1) doubles to variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CmathLibV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def cmathlib_ctopolardeg(self) -> List[float]:
        """Convert complex number (Argument1 and Argument2) to magnitude and angle, degrees. Returns variant array of
         two doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CmathLibV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def cmathlib_pdegtocomplex(self):
        """Convert magnitude, angle in degrees (Argument1 and Argument2) to a complex number. Returns variant array of
         two doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CmathLibV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
