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

    def isources_read_amps(self) -> float:
        """Gets the magnitude of the Isource in Amps."""
        return float(self.dss_obj.IsourceF(ctypes.c_int32(0), ctypes.c_double(0)))

    def isources_write_amps(self, argument: float) -> float:
        """Sets the magnitude of the Isource in Amps."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.IsourceF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def isources_read_angle_deg(self) -> float:
        """Gets the phase angle of the Isource in degrees."""
        return float(self.dss_obj.IsourceF(ctypes.c_int32(2), ctypes.c_double(0)))

    def isources_write_angle_deg(self, argument: float) -> float:
        """Sets the phase angle of the Isource in degrees."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.IsourceF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def isources_read_frequency(self) -> float:
        """Gets the frequency of the Isource in Hz."""
        return float(self.dss_obj.IsourceF(ctypes.c_int32(4), ctypes.c_double(0)))

    def isources_write_frequency(self, argument: float) -> float:
        """Sets the frequency of the Isource in Hz."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.IsourceF(ctypes.c_int32(5), ctypes.c_double(argument)))
