# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base
from py_dss_interface.models.Lines import LinesS
from py_dss_interface.models.Text.Text import Text


class LinesF(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are doubles.

    The structure of the interface is as follows:
        double LinesF(int32_t Parameter, double argument)

    This interface returns a floating point number, the variable “parameter” is used to specify the property of the
    class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def lines_read_length(self) -> float:
        """Gets the length of line section in units compatible with the LineCode definition."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(0), ctypes.c_double(0)))

    def lines_write_length(self, argument: float) -> float:
        """Sets the length of line section in units compatible with the LineCode definition."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def lines_read_r1(self) -> float:
        """Gets the positive sequence resistance, ohm per unit length."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def lines_write_r1(self, argument: float) -> float:
        """Sets the positive sequence resistance, ohm per unit length."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def lines_read_x1(self) -> float:
        """Gets the positive sequence reactance, ohm per unit length."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(4), ctypes.c_double(0)))

    def lines_write_x1(self, argument: float) -> float:
        """Sets the positive sequence reactance, ohm per unit length."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def lines_read_r0(self) -> float:
        """Gets the zero sequence resistance, ohm per unit length."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(6), ctypes.c_double(0)))

    def lines_write_r0(self, argument: float) -> float:
        """Sets the zero sequence resistance, ohm per unit length."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def lines_read_x0(self) -> float:
        """Gets the zero sequence reactance, ohm per unit length."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(8), ctypes.c_double(0)))

    def lines_write_x0(self, argument: float) -> float:
        """Sets the zero sequence reactance, ohm per unit length."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def lines_read_c1(self) -> float:
        """Gets the positive sequence capacitance, nanofarads per unit length."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(10), ctypes.c_double(0)))

    def lines_write_c1(self, argument: float) -> float:
        """Sets the positive sequence capacitance, nanofarads per unit length."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def lines_read_c0(self) -> float:
        """Gets the zero sequence capacitance, nanofarads per unit length."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(12), ctypes.c_double(0)))

    def lines_write_c0(self, argument: float) -> float:
        """Sets the zero sequence capacitance, nanofarads per unit length."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def lines_read_norm_amps(self) -> float:
        """Gets the normal ampere rating of line section."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(14), ctypes.c_double(0)))

    def lines_write_norm_amps(self, argument: float) -> float:
        """Sets the normal ampere rating of Line."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(15), ctypes.c_double(argument)))

    def lines_read_emerg_amps(self) -> float:
        """Gets the emergency (maximum) ampere rating of Line."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(16), ctypes.c_double(0)))

    def lines_write_emerg_amps(self, argument: float) -> float:
        """Sets the emergency (maximum) ampere rating of Line."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(17), ctypes.c_double(argument)))

    def lines_read_rg(self) -> float:
        """Gets the earth return value used to compute line impedances at power frequency."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(18), ctypes.c_double(0)))

    def lines_write_rg(self, argument: float) -> float:
        """Sets the earth return value used to compute line impedances at power frequency."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(19), ctypes.c_double(argument)))

    def lines_read_xg(self) -> float:
        """Gets the earth return reactance value used to compute line impedances at power frequency."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(20), ctypes.c_double(0)))

    def lines_write_xg(self, argument: float) -> float:
        """Sets the earth return reactance value used to compute line impedances at power frequency."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(21), ctypes.c_double(argument)))

    def lines_read_rho(self) -> float:
        """Gets the earth resistivity, m-ohms."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(22), ctypes.c_double(0)))

    def lines_write_rho(self, argument: float) -> float:
        """Sets the earth resistivity, m-ohms."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LinesF(ctypes.c_int32(23), ctypes.c_double(argument)))

    def lines_read_season_rating(self) -> float:
        """Returns the rating for the current season (in Amps) if the SeasonalRatings option is active."""
        return float(self.dss_obj.LinesF(ctypes.c_int32(24), ctypes.c_double(0)))
