# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapControlsF(Base):

    def cap_controls_read_ct_ratio(self):
        """Gets the transducer ratio current to control current."""
        return self.get_float(0, 0)

    def cap_controls_write_ct_ratio(self, argument):
        """Sets the transducer ratio current to control current."""
        return self.get_float(1, argument)

    def cap_controls_read_pt_ratio(self):
        """Gets the transducer ratio from primary feeder to control voltage."""
        return self.get_float(2, 0)

    def cap_controls_write_pt_ratio(self, argument):
        """Sets the transducer ratio from primary feeder to control voltage."""
        return self.get_float(3, argument)

    def cap_controls_read_on_setting(self):
        """Gets the threshold to arm or switch on a step. See Mode for Units."""
        return self.get_float(4, 0)

    def cap_controls_write_on_setting(self, argument):
        """Sets the threshold to arm or switch on a step. See Mode for Units."""
        return self.get_float(5, argument)

    def cap_controls_read_off_setting(self):
        """Gets the threshold to switch off a step. See Mode for Units."""
        return self.get_float(6, 0)

    def cap_controls_write_off_setting(self, argument):
        """Sets the threshold to switch off a step. See Mode for Units."""
        return self.get_float(7, argument)

    def cap_controls_read_vmax(self):
        """Gets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        return self.get_float(8, 0)

    def cap_controls_write_vmax(self, argument):
        """Sets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        return self.get_float(9, argument)

    def cap_controls_read_vmin(self):
        """Gets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        return self.get_float(10, 0)

    def cap_controls_write_vmin(self, argument):
        """Sets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        return self.get_float(11, argument)

    def cap_controls_read_delay(self):
        """Gets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        return self.get_float(12, 0)

    def cap_controls_write_delay(self, argument):
        """Sets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        return self.get_float(13, argument)

    def cap_controls_read_delay_off(self):
        """Gets the time delay [s] before switching off a step. Control may reset before actually switching."""
        return self.get_float(14, 0)

    def cap_controls_write_delay_off(self, argument):
        """Sets the time delay [s] before switching off a step. Control may reset before actually switching."""
        return self.get_float(15, argument)

    def cap_controls_read_dead_time(self):
        """Gets the time delay [s] after switching off a step. Control may reset before actually switching."""
        return self.get_float(16, 0)

    def cap_controls_write_dead_time(self, argument):
        """Sets the time delay [s] after switching off a step. Control may reset before actually switching.."""
        return self.get_float(17, argument)
