# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LinesF(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are doubles.

    The structure of the interface is as follows:
        double LinesF(int32_t Parameter, double argument)

    This interface returns a floating point number, the variable “parameter” is used to specify the property of the
    class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _length_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _length_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _r1_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _r1_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def _x1_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _x1_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def _r0_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(6), ctypes.c_double(0)))

    def _r0_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def _x0_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(8), ctypes.c_double(0)))

    def _x0_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def _c1_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(10), ctypes.c_double(0)))

    def _c1_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def _c0_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(12), ctypes.c_double(0)))

    def _c0_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def _norm_amps_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(14), ctypes.c_double(0)))

    def _norm_amps_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(15), ctypes.c_double(argument)))

    def _emerg_amps_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(16), ctypes.c_double(0)))

    def _emerg_amps_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(17), ctypes.c_double(argument)))

    def _rg_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(18), ctypes.c_double(0)))

    def _rg_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(19), ctypes.c_double(argument)))

    def _xg_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(20), ctypes.c_double(0)))

    def _xg_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(21), ctypes.c_double(argument)))

    def _rho_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(22), ctypes.c_double(0)))

    def _rho_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.LinesF(ctypes.c_int32(23), ctypes.c_double(argument)))

    def _season_rating_read(self) -> float:
        return float(self._dss_obj.LinesF(ctypes.c_int32(24), ctypes.c_double(0)))
