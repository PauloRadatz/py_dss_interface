# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import Tuple

from py_dss_interface.models.CMathLib.CMathLibF import CMathLibF
from py_dss_interface.models.CMathLib.CMathLibV import CMathLibV


class CMathLib(CMathLibF, CMathLibV):
    """
    This interface implements the CmathLib (ICmathLib) interface of OpenDSS by declaring 2 procedures for accessing
    the different properties included in this interface: CMathLibF, CMathLibV.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @staticmethod
    def cabs(arg_real: float, arg_imaginary: float) -> float:
        """Returns the absolute value of complex number given in real (Argument1) and imaginary (Argument2) doubles."""
        return CMathLibF._cabs(arg_real, arg_imaginary)

    @staticmethod
    def cdang(arg_real: float, arg_imaginary: float) -> float:
        """Returns the angle, in degrees, of a complex number specified as two doubles: Real part (Argument1) and
                imaginary part (Argument2)."""
        return CMathLibF._cdang(arg_real, arg_imaginary)

    @staticmethod
    def cmplx(real_part: float, imag_part: float) -> complex:
        """Convert real (Argument1) and imaginary (Argument1) doubles to variant array of doubles."""
        return CMathLibV._cmplx(real_part, imag_part)

    @staticmethod
    def ctopolardeg(double_real: float, double_imag: float) -> Tuple[float, float]:
        """Convert complex number (Argument1 and Argument2) to magnitude and angle, degrees. Returns variant array of
                 two doubles."""
        return CMathLibV._ctopolardeg(double_real, double_imag)

    @staticmethod
    def pdegtocomplex(double_real: float, double_imag: float) -> complex:
        """Convert magnitude, angle in degrees (Argument1 and Argument2) to a complex number. Returns variant array of
                 two doubles."""
        return CMathLibV._pdegtocomplex(double_real, double_imag)
