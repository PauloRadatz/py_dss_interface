# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class SettingsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double SettingsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def _allocation_factors(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _norm_vmin_pu_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(1), ctypes.c_double(0)))

    def _norm_vmin_pu_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(2), ctypes.c_double(argument)))

    def _norm_vmax_pu_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(3), ctypes.c_double(0)))

    def _norm_vmax_pu_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(4), ctypes.c_double(argument)))

    def _emerg_vmin_pu_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(5), ctypes.c_double(0)))

    def _emerg_vmin_pu_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(6), ctypes.c_double(argument)))

    def _emerg_vmax_pu_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(7), ctypes.c_double(0)))

    def _emerg_vmax_pu_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(8), ctypes.c_double(argument)))

    def _ue_weight_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(9), ctypes.c_double(0)))

    def _ue_weight_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(10), ctypes.c_double(argument)))

    def _loss_weight_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(11), ctypes.c_double(0)))

    def _loss_weight_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(12), ctypes.c_double(argument)))

    def _price_signal_read(self) -> float:
        return float(self._dss_obj.SettingsF(ctypes.c_int32(13), ctypes.c_double(0)))

    def _price_signal_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.SettingsF(ctypes.c_int32(14), ctypes.c_double(argument)))
