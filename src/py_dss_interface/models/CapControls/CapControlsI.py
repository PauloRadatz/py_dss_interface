# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapControlsI(Base):

    def cap_controls_first(self):
        """Sets the first CapControl active. Returns 0 if no more."""
        return self.get_integer(0, 0)

    def cap_controls_next(self):
        """Sets the next CapControl active. Returns 0 if no more."""
        return self.get_integer(1, 0)

    def cap_controls_read_mode(self):
        """Gets the type of automatic controller (see manual for details)."""
        return self.get_integer(2, 0)

    def cap_controls_write_mode(self, argument):
        """Sets the type of automatic controller (see manual for details)."""
        return self.get_integer(3, argument)

    def cap_controls_read_monitored_term(self):
        """Gets the terminal number on the element that PT and CT are connected to."""
        return self.get_integer(4, 0)

    def cap_controls_write_monitored_term(self, argument):
        """Sets the terminal number on the element that PT and CT are connected to."""
        return self.get_integer(5, argument)

    def cap_controls_read_use_volt_override(self):
        """Gets if Vmin and Vmax are enabled to override the control Mode."""
        return self.get_integer(6, 0)

    def cap_controls_write_use_volt_override(self, argument):
        """Sets if enables Vmin and Vmax to override the control Mode."""
        return self.get_integer(7, argument)

    def cap_controls_count(self):
        """Gets the number of CapControls in Active Circuit."""
        return self.get_integer(8, 0)
