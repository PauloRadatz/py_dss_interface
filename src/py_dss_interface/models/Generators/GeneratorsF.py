# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class GeneratorsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double GeneratorsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def _kv(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _kv_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _kw(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _kw_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def _kvar(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _kvar_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def _pf(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(6), ctypes.c_double(0)))

    def _pf_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def _kva_rated(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(8), ctypes.c_double(0)))

    def _kva_rated_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def _vmax_pu(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(10), ctypes.c_double(0)))

    def _vmax_pu_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def _vmin_pu(self) -> float:
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(12), ctypes.c_double(0)))

    def _vmin_pu_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.GeneratorsF(ctypes.c_int32(13), ctypes.c_double(argument)))
