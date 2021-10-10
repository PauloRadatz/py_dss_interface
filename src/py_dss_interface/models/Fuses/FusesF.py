# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class FusesF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double FusesF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def fuses_read_rated_current(self) -> float:
        """Gets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0, Multiply current values of
        TCC curve by this to get actual amps. """
        result = float(self.dss_obj.FusesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def fuses_write_rated_current(self, argument: float) -> float:
        """Sets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0, Multiply current values of
        TCC curve by this to get actual amps. """
        argument = Base.check_float_param(argument, 1.0)
        return float(self.dss_obj.FusesF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def fuses_read_delay(self) -> float:
        """Gets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default
        is 0. """
        return float(self.dss_obj.FusesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def fuses_write_delay(self, argument: float) -> float:
        """Sets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default
        is 0. """
        argument = Base.check_float_param(argument, 0.0)
        return float(self.dss_obj.FusesF(ctypes.c_int32(3), ctypes.c_double(argument)))
