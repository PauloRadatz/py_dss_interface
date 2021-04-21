# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CMathLib(Base):

    # CmathLibF (Float)
    def cmathlib_cabs(self):
        """Returns the absolute value of complex number given in real (Argument1) and imaginary (Argument2) doubles."""
        result = float(self.dss_obj.CmathLibF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def cmathlib_cdang(self):
        """Returns the angle, in degrees, of a complex number specified as two doubles: Real part (Argument1) and
        imaginary part (Argument2)."""
        result = float(self.dss_obj.CmathLibF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    # CmathLibV (Variant)
    def cmathlib_cmplx(self):
        """Convert real (Argument1) and imaginary (Argument1) doubles to variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CmathLibV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def cmathlib_ctopolardeg(self):
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
