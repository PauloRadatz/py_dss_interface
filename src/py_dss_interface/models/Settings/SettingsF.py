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

    def settings_allocation_factors(self) -> float:
        """Sets all load allocation factors for all loads defined by XFKVA property to this value."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def settings_read_norm_vmin_pu(self) -> float:
        """Gets the per unit minimum voltage for Normal conditions."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(1), ctypes.c_double(0)))

    def settings_write_norm_vmin_pu(self, argument) -> float:
        """Sets the per unit minimum voltage for Normal conditions."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(2), ctypes.c_double(argument)))

    def settings_read_norm_vmax_pu(self) -> float:
        """Gets the per unit maximum voltage for Normal conditions."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(3), ctypes.c_double(0)))

    def settings_write_norm_vmax_pu(self, argument) -> float:
        """Sets the per unit maximum voltage for Normal conditions."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(4), ctypes.c_double(argument)))

    def settings_read_emerg_vmin_pu(self) -> float:
        """Gets the per unit minimum voltage for Emergency conditions."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(5), ctypes.c_double(0)))

    def settings_write_emerg_vmin_pu(self, argument) -> float:
        """Sets the per unit minimum voltage for Emergency conditions."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(6), ctypes.c_double(argument)))

    def settings_read_emerg_vmax_pu(self) -> float:
        """Gets the per unit maximum voltage for Emergency conditions."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(7), ctypes.c_double(0)))

    def settings_write_emerg_vmax_pu(self, argument) -> float:
        """Sets the per unit maximum voltage for Emergency conditions."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(8), ctypes.c_double(argument)))

    def settings_read_ue_weight(self) -> float:
        """Gets the weighting factor applied to UE register values."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(9), ctypes.c_double(0)))

    def settings_write_ue_weight(self, argument) -> float:
        """Sets the weighting factor applied to UE register values."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(10), ctypes.c_double(argument)))

    def settings_read_loss_weight(self) -> float:
        """Gets the weighting factor applied to Loss register values."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(11), ctypes.c_double(0)))

    def settings_write_loss_weight(self, argument) -> float:
        """Sets the weighting factor applied to Loss register values."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(12), ctypes.c_double(argument)))

    def settings_read_price_signal(self) -> float:
        """Gets the price signal for the circuit."""
        return float(self.dss_obj.SettingsF(ctypes.c_int32(13), ctypes.c_double(0)))

    def settings_write_price_signal(self, argument) -> float:
        """Sets the price signal for the circuit."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.SettingsF(ctypes.c_int32(14), ctypes.c_double(argument)))
