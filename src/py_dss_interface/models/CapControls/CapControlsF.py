# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CapControlsF(Base):

    def cap_controls_float(self, first, second):
        return float(self.dss_obj.CapControlsF(ctypes.c_int32(first), ctypes.c_double(second)))

    def read_ct_ratio(self):
        """Gets the transducer ratio current to control current."""
        return self.cap_controls_float(0, 0)

    def write_ct_ratio(self, argument):
        """Sets the transducer ratio current to control current."""
        return self.cap_controls_float(1, argument)

    def read_pt_ratio(self):
        """Gets the transducer ratio from primary feeder to control voltage."""
        return self.cap_controls_float(2, 0)

    def write_pt_ratio(self, argument):
        """Sets the transducer ratio from primary feeder to control voltage."""
        return self.cap_controls_float(3, argument)

    def read_on_setting(self):
        """Gets the threshold to arm or switch on a step. See Mode for Units."""
        return self.cap_controls_float(4, 0)

    def write_on_setting(self, argument):
        """Sets the threshold to arm or switch on a step. See Mode for Units."""
        return self.cap_controls_float(5, argument)

    def read_off_setting(self):
        """Gets the threshold to switch off a step. See Mode for Units."""
        return self.cap_controls_float(6, 0)

    def write_off_setting(self, argument):
        """Sets the threshold to switch off a step. See Mode for Units."""
        return self.cap_controls_float(7, argument)

    def read_vmax(self):
        """Gets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        return self.cap_controls_float(8, 0)

    def write_vmax(self, argument):
        """Sets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        return self.cap_controls_float(9, argument)

    def read_vmin(self):
        """Gets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        return self.cap_controls_float(10, 0)

    def write_vmin(self, argument):
        """Sets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        return self.cap_controls_float(11, argument)

    def read_delay(self):
        """Gets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        return self.cap_controls_float(12, 0)

    def write_delay(self, argument):
        """Sets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        return self.cap_controls_float(13, argument)

    def read_delay_off(self):
        """Gets the time delay [s] before switching off a step. Control may reset before actually switching."""
        return self.cap_controls_float(14, 0)

    def write_delay_off(self, argument):
        """Sets the time delay [s] before switching off a step. Control may reset before actually switching."""
        return self.cap_controls_float(15, argument)

    def read_dead_time(self):
        """Gets the time delay [s] after switching off a step. Control may reset before actually switching."""
        return self.cap_controls_float(16, 0)

    def write_dead_time(self, argument):
        """Sets the time delay [s] after switching off a step. Control may reset before actually switching.."""
        return self.cap_controls_float(17, argument)
