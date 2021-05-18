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

    def reclosers_read_phase_trip(self) -> float:
        """Gets the phase trip curve multiplier or actual amps."""
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(0), ctypes.c_double(0)))

    def reclosers_write_phase_trip(self, argument) -> float:
        """Sets the phase trip curve multiplier or actual amps."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def reclosers_read_phase_inst(self) -> float:
        """Gets the phase instantaneous curve multiplier or actual amps."""
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(2), ctypes.c_double(0)))

    def reclosers_write_phase_inst(self, argument) -> float:
        """Sets the phase instantaneous curve multiplier or actual amps."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def reclosers_read_ground_trip(self) -> float:
        """Gets the ground (3I0) trip multiplier or actual amps."""
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(4), ctypes.c_double(0)))

    def reclosers_write_ground_trip(self, argument) -> float:
        """Sets the ground (3I0) trip multiplier or actual amps."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def reclosers_read_ground_inst(self) -> float:
        """Gets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps."""
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(6), ctypes.c_double(0)))

    def reclosers_write_ground_inst(self, argument) -> float:
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.ReclosersF(ctypes.c_int32(7), ctypes.c_double(argument)))
