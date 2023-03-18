# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class ISourcesF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double IsourcesF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def _amps_read(self) -> float:
        return float(self._dss_obj.IsourceF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _amps_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.IsourceF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _angle_deg_read(self) -> float:
        return float(self._dss_obj.IsourceF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _angle_deg_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.IsourceF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def _frequency_read(self) -> float:
        return float(self._dss_obj.IsourceF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _frequency_write(self, argument: float) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.IsourceF(ctypes.c_int32(5), ctypes.c_double(argument)))
