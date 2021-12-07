# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class RegControlsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double RegControlsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def regcontrols_read_ct_primary(self) -> float:
        """Gets the CT primary ampere rating (secondary is 0.2 amperes)."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def regcontrols_write_ct_primary(self, argument) -> float:
        """Sets the CT primary ampere rating (secondary is 0.2 amperes)."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def regcontrols_read_pt_ratio(self) -> float:
        """Gets the PT ratio for voltage control settings."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def regcontrols_write_pt_ratio(self, argument) -> float:
        """Sets the PT ratio for voltage control settings."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def regcontrols_read_forward_r(self) -> float:
        """Gets the LDC R settings in Volts."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(4), ctypes.c_double(0)))

    def regcontrols_write_forward_r(self, argument) -> float:
        """Sets the LDC R settings in Volts."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def regcontrols_read_forward_x(self) -> float:
        """Gets the LDC X settings in Volts."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(6), ctypes.c_double(0)))

    def regcontrols_write_forward_x(self, argument) -> float:
        """Sets sets the LDC X settings in Volts."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def regcontrols_read_reverse_r(self) -> float:
        """Gets the reverse LDC R settings in Volts."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(8), ctypes.c_double(0)))

    def regcontrols_write_reverse_r(self, argument) -> float:
        """Sets the reverse LDC R settings in Volts."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def regcontrols_read_reverse_x(self) -> float:
        """Gets the reverse LDC X settings in Volts."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(10), ctypes.c_double(0)))

    def regcontrols_write_reverse_x(self, argument) -> float:
        """Sets the reverse LDC X settings in Volts."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def regcontrols_read_delay(self) -> float:
        """Gets the time delay [s] after arming before the first tap change.
        Control may reset before actually changing taps."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(12), ctypes.c_double(0)))

    def regcontrols_write_delay(self, argument) -> float:
        """Sets the time delay [s] after arming before the first tap change. Control may reset before actually
        changing taps. """
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def regcontrols_read_tap_delay(self) -> float:
        """Gets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
        taps."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(14), ctypes.c_double(0)))

    def regcontrols_write_tap_delay(self, argument) -> float:
        """Sets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
        taps."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(15), ctypes.c_double(argument)))

    def regcontrols_read_voltage_limit(self) -> float:
        """Gets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(16), ctypes.c_double(0)))

    def regcontrols_write_voltage_limit(self, argument) -> float:
        """Sets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(17), ctypes.c_double(argument)))

    def regcontrols_read_forward_band(self) -> float:
        """Gets the regulation bandwidth in forward direction, centered on Vreg."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(18), ctypes.c_double(0)))

    def regcontrols_write_forward_band(self, argument) -> float:
        """Sets the regulation bandwidth in forward direction, centered on Vreg."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(19), ctypes.c_double(argument)))

    def regcontrols_read_forward_vreg(self) -> float:
        """Gets the target voltage in the forward direction, on PT secondary base."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(20), ctypes.c_double(0)))

    def regcontrols_write_forward_vreg(self, argument) -> float:
        """Sets the target voltage in the forward direction, on PT secondary base."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(21), ctypes.c_double(argument)))

    def regcontrols_read_reverse_band(self) -> float:
        """Gets the bandwidth in reverse direction, centered on reverse Vreg."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(22), ctypes.c_double(0)))

    def regcontrols_write_reverse_band(self, argument) -> float:
        """Sets the bandwidth in reverse direction, centered on reverse Vreg."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(23), ctypes.c_double(argument)))

    def regcontrols_read_reverse_vreg(self) -> float:
        """Gets the target voltage in the reverse direction, on PT secondary base."""
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(24), ctypes.c_double(0)))

    def regcontrols_write_reverse_vreg(self, argument) -> float:
        """Sets the target voltage in the reverse direction, on PT secondary base."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.RegControlsF(ctypes.c_int32(25), ctypes.c_double(argument)))
