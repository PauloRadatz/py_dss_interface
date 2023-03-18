# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models.Base import Base


class LoadsF(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are floating point
    numbers (double).

    The structure of the interface is as follows:
        double DSSLoadsF(int32_t Parameter,double Argument);

    This interface returns a Double (IEEE 754 64 bits), the variable “parameter” (Integer) is used to specify the
    property of the class to be used and the variable “argument” (double) can be used to modify the value of the
    property when necessary. Reading and writing properties are separated and require a different parameter number to
    be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _kw(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _kw_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _kv(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _kv_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def _kvar(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _kvar_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def _pf(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(6), ctypes.c_double(0)))

    def _pf_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def _pct_mean(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(8), ctypes.c_double(0)))

    def _pct_mean_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def _pct_std_dev(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(10), ctypes.c_double(0)))

    def _pct_std_dev_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def _allocation_factor(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(12), ctypes.c_double(0)))

    def _allocation_factor_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def _c_factor(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(14), ctypes.c_double(0)))

    def _c_factor_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(15), ctypes.c_double(argument)))

    def _cvr_watts(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(16), ctypes.c_double(0)))

    def _cvr_watts_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(17), ctypes.c_double(argument)))

    def _cvr_vars(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(18), ctypes.c_double(0)))

    def _cvr_vars_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(19), ctypes.c_double(argument)))

    def _kva(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(20), ctypes.c_double(0)))

    def _kva_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(21), ctypes.c_double(argument)))

    def _kwh(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(22), ctypes.c_double(0)))

    def _kwh_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(23), ctypes.c_double(argument)))

    def _kwh_days(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(24), ctypes.c_double(0)))

    def _kwh_days_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(25), ctypes.c_double(argument)))

    def _r_neut(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(26), ctypes.c_double(0)))

    def _r_neut_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(27), ctypes.c_double(argument)))

    def _vmax_pu(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(28), ctypes.c_double(0)))

    def _vmax_pu_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(29), ctypes.c_double(argument)))

    def _vmin_emerg(self) -> float:
         return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(30), ctypes.c_double(0)))

    def _vmin_emerg_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(31), ctypes.c_double(argument)))

    def _vmin_norm(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(32), ctypes.c_double(0)))

    def _vmin_norm_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(33), ctypes.c_double(argument)))

    def _vmin_pu(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(34), ctypes.c_double(0)))

    def _vmin_pu_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(35), ctypes.c_double(argument)))

    def _xfkva(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(36), ctypes.c_double(0)))

    def _xfkva_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(37), ctypes.c_double(argument)))

    def _x_neut(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(38), ctypes.c_double(0)))

    def _x_neut_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(39), ctypes.c_double(argument)))

    def _rl(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(40), ctypes.c_double(0)))

    def _rl_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(41), ctypes.c_double(argument)))

    def _rel_weight(self) -> float:
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(42), ctypes.c_double(0)))

    def _rel_weight_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.DSSLoadsF(ctypes.c_int32(43), ctypes.c_double(argument)))
