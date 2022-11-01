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
        return CMathLibF._cabs(arg_real, arg_imaginary)

    @staticmethod
    def cdang(arg_real: float, arg_imaginary: float) -> float:
        return CMathLibF._cdang(arg_real, arg_imaginary)

    @staticmethod
    def cmplx(real_part: float, imag_part: float) -> complex:
        return CMathLibV._cmplx(real_part, imag_part)

    @staticmethod
    def ctopolardeg(double_real: float, double_imag: float) -> Tuple[float, float]:
        return CMathLibV._ctopolardeg(double_real, double_imag)

    @staticmethod
    def pdegtocomplex(double_real: float, double_imag: float) -> complex:
        return CMathLibV._pdegtocomplex(double_real, double_imag)
