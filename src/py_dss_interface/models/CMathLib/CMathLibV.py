# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import cmath
from typing import Tuple

from py_dss_interface.models.Base import Base


class CMathLibV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CmathLibV(int32_t Parameter, double Argument1, Argument2, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def cmathlib_cmplx(self, real_part: float, imag_part: float) -> complex:
        """Convert real (Argument1) and imaginary (Argument1) doubles to variant array of doubles."""
        return complex(real_part, imag_part)

    def cmathlib_ctopolardeg(self, double_real: float, double_imag: float) -> Tuple[float, float]:
        """Convert complex number (Argument1 and Argument2) to magnitude and angle, degrees. Returns variant array of
         two doubles."""
        z = complex(double_real, double_imag)
        r, theta = cmath.polar(z)
        return r, theta

    def cmathlib_pdegtocomplex(self, double_real: float, double_imag: float) -> complex:
        """Convert magnitude, angle in degrees (Argument1 and Argument2) to a complex number. Returns variant array of
         two doubles."""
        return cmath.rect(double_real, double_imag)
