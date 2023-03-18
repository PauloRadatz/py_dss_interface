
# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import cmath
from typing import Tuple

from py_dss_interface.models.Base import Base
from py_dss_interface.utils.Error import Error


class CMathLibV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CmathLibV(int32_t Parameter, double Argument1, Argument2, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    @staticmethod
    def _cmplx(real_part: float, imag_part: float) -> complex:
        return complex(real_part, imag_part)

    @staticmethod
    def _ctopolardeg(double_real: float, double_imag: float) -> Tuple[float, float]:
        z = complex(double_real, double_imag)
        r, theta = cmath.polar(z)
        return r, theta

    @staticmethod
    def _pdegtocomplex(double_real: float, double_imag: float) -> complex:
        # return cmath.rect(double_real, double_imag)
        Error.method_not_working("pdegtocomplex")
