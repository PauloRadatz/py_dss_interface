# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LineCodesF(Base):
    """
    This interface can be used to read/modify the properties of the LineCode Class where the values are doubles.

    The structure of the interface is as follows:
        double LineCodesF(int32_t Parameter, double argument)

    This interface returns a floating point number, the variable “parameter” is used to specify the property of the
    class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _r1_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _r1_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(1), ctypes.c_double(arg)))

    def _x1_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _x1_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(3), ctypes.c_double(arg)))

    def _r0_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _r0_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(5), ctypes.c_double(arg)))

    def _x0_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(6), ctypes.c_double(0)))

    def _x0_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(7), ctypes.c_double(arg)))

    def _c1_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(8), ctypes.c_double(0)))

    def _c1_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(9), ctypes.c_double(arg)))

    def _c0_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(10), ctypes.c_double(0)))

    def _c0_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(11), ctypes.c_double(arg)))

    def _norm_amps_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(12), ctypes.c_double(0)))

    def _norm_amps_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(13), ctypes.c_double(arg)))

    def _emerg_amps_read(self) -> float:
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(14), ctypes.c_double(0)))

    def _emerg_amps_write(self, arg: float) -> float:
        arg = Base._check_float_param(arg)
        return float(self._dss_obj.LineCodesF(ctypes.c_int32(15), ctypes.c_double(arg)))
