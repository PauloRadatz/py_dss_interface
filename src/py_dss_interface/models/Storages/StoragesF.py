# -*- coding: utf-8 -*-
# @Time    : 5/6/2024 7:31 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : StoragesF.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models.Base import Base


class StoragesF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double SwtControlsF(int32_t Parameter, double Argument);

    This interface returns a floating point number (64 bits) with the result of the query according to the value of
    the variable Parameter, which can be one of the following.
    """

    def _pu_soc_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _pu_soc_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _amp_limit_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _amp_limit_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def _amp_limit_gain_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _amp_limit_gain_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def _charge_trigger_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(6), ctypes.c_double(0)))

    def _charge_trigger_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def _discharge_trigger_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(8), ctypes.c_double(0)))

    def _discharge_trigger_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def _eff_charge_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(10), ctypes.c_double(0)))

    def _eff_charge_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def _eff_discharge_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(12), ctypes.c_double(0)))

    def _eff_discharge_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def _kp_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(14), ctypes.c_double(0)))

    def _kp_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(15), ctypes.c_double(argument)))

    def _kv_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(16), ctypes.c_double(0)))

    def _kv_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(17), ctypes.c_double(argument)))

    def _kva_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(18), ctypes.c_double(0)))

    def _kva_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(19), ctypes.c_double(argument)))

    def _kvar_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(20), ctypes.c_double(0)))

    def _kvar_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(21), ctypes.c_double(argument)))

    def _kvdc_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(22), ctypes.c_double(0)))

    def _kvdc_write(self, argument) -> float: # TODO waiting Davis to answer the order
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(23), ctypes.c_double(argument)))

    def _kw_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(24), ctypes.c_double(0)))

    def _kw_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(25), ctypes.c_double(argument)))

    def _kwh_rated_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(26), ctypes.c_double(0)))

    def _kwh_rated_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(27), ctypes.c_double(argument)))

    def _kw_rated_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(28), ctypes.c_double(0)))

    def _kw_rated_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(29), ctypes.c_double(argument)))

    def _limit_current_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(30), ctypes.c_double(0)))

    def _limit_current_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(31), ctypes.c_double(argument)))

    def _pf_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(32), ctypes.c_double(0)))

    def _pf_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(33), ctypes.c_double(argument)))

    def _pi_tol_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(34), ctypes.c_double(0)))

    def _pi_tol_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(35), ctypes.c_double(argument)))

    def _safe_voltage_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(36), ctypes.c_double(0)))

    def _safe_voltage_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(37), ctypes.c_double(argument)))

    def _time_charge_trig_read(self) -> float:
        return float(self._dss_obj.StoragesF(ctypes.c_int32(38), ctypes.c_double(0)))

    def _time_charge_trig_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.StoragesF(ctypes.c_int32(39), ctypes.c_double(argument)))

