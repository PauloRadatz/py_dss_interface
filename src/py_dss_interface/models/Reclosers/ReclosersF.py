# -*- coding: iso-8859-15 -*-

import ctypes

from py_dss_interface.models.Base import Base


class ReclosersF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double ReclosersF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def _phase_trip_read(self) -> float:
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _phase_trip_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _phase_inst_read(self) -> float:
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _phase_inst_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def _ground_trip_read(self) -> float:
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(4), ctypes.c_double(0)))

    def _ground_trip_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def _ground_inst_read(self) -> float:
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(6), ctypes.c_double(0)))

    def _ground_inst_write(self, argument) -> float:
        argument = Base._check_float_param(argument)
        return float(self._dss_obj.ReclosersF(ctypes.c_int32(7), ctypes.c_double(argument)))
