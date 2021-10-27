# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CapControlsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double CapControlsF(int32_t Parameter, double Argument);

    This interface returns a floating point number (64 bits) with the result of the query according to the value of
    the variable Parameter, which can be one of the following.
    """

    def capcontrols_read_ct_ratio(self) -> float:
        """Gets the transducer ratio current to control current."""
        return self.dss_obj.CapControlsF(0, 0)

    def capcontrols_write_ct_ratio(self, argument: float) -> float:
        """Sets the transducer ratio current to control current."""
        try:
            return self.dss_obj.CapControlsF(1, argument)
        except Exception as e:
            print(f'capcontrols_write_ct_ratio: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_pt_ratio(self) -> float:
        """Gets the transducer ratio from primary feeder to control voltage."""
        return self.dss_obj.CapControlsF(2, 0)

    def capcontrols_write_pt_ratio(self, argument: float) -> float:
        """Sets the transducer ratio from primary feeder to control voltage."""
        try:
            return self.dss_obj.CapControlsF(3, argument)
        except Exception as e:
            print(f'capcontrols_write_pt_ratio: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_on_setting(self) -> float:
        """Gets the threshold to arm or switch on a step. See Mode for Units."""
        return self.dss_obj.CapControlsF(4, 0)

    def capcontrols_write_on_setting(self, argument: float) -> float:
        """Sets the threshold to arm or switch on a step. See Mode for Units."""
        try:
            return self.dss_obj.CapControlsF(5, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_on_setting: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_off_setting(self) -> float:
        """Gets the threshold to switch off a step. See Mode for Units."""
        return self.dss_obj.CapControlsF(6, 0)

    def capcontrols_write_off_setting(self, argument: float) -> float:
        """Sets the threshold to switch off a step. See Mode for Units."""
        try:
            return self.dss_obj.CapControlsF(7, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_off_setting: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_vmax(self) -> float:
        """Gets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        return self.dss_obj.CapControlsF(8, 0)

    def capcontrols_write_vmax(self, argument: float) -> float:
        """Sets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        try:
            return self.dss_obj.CapControlsF(9, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_vmax: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_vmin(self) -> float:
        """Gets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        return self.dss_obj.CapControlsF(10, 0)

    def capcontrols_write_vmin(self, argument: float) -> float:
        """Sets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        try:
            return self.dss_obj.CapControlsF(11, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_vmin: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_delay(self) -> float:
        """Gets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        return self.dss_obj.CapControlsF(12, 0)

    def capcontrols_write_delay(self, argument: float) -> float:
        """Sets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        try:
            return self.dss_obj.CapControlsF(13, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_delay: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_delay_off(self) -> float:
        """Gets the time delay [s] before switching off a step. Control may reset before actually switching."""
        return self.dss_obj.CapControlsF(14, 0)

    def capcontrols_write_delay_off(self, argument: float) -> float:
        """Sets the time delay [s] before switching off a step. Control may reset before actually switching."""
        try:
            return self.dss_obj.CapControlsF(15, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_delay_off: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_dead_time(self) -> float:
        """Gets the time delay [s] after switching off a step. Control may reset before actually switching."""
        return self.dss_obj.CapControlsF(16, 0)

    def capcontrols_write_dead_time(self, argument: float) -> float:
        """Sets the time delay [s] after switching off a step. Control may reset before actually switching.."""
        try:
            return self.dss_obj.CapControlsF(17, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_dead_time: Check if exist at least one CapControl!. {e}')
