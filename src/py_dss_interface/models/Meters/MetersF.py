# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MetersF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double MetersF(int32_t Parameter, double Argument);

    This interface returns a floating point number (64 bits) according to the number sent in the variable
    “parameter”. The parameter can be one of the following.
    """

    def _saifi(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _saifi_kw(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(1), ctypes.c_double(0)))

    # TODO include in test
    def _saidi(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _cust_interrupts(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(3), ctypes.c_double(0)))

    def _avg_repair_time(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _fault_rate_x_repair_hrs(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(5), ctypes.c_double(0)))

    def _sum_branch_flt_rates(self) -> float:
        return float(self._dss_obj.MetersF(ctypes.c_int32(6), ctypes.c_double(0)))
