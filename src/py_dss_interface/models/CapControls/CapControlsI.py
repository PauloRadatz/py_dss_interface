# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CapControlsI(Base):

    def cap_controls_integer(self, first, second):
        return int(self.dss_obj.CapControlsI(ctypes.c_int32(first), ctypes.c_int32(second)))

    def first(self):
        """Sets the first CapControl active. Returns 0 if no more."""
        return self.cap_controls_integer(0, 0)

    def next(self):
        """Sets the next CapControl active. Returns 0 if no more."""
        return self.cap_controls_integer(1, 0)

    def read_mode(self):
        """Gets the type of automatic controller (see manual for details)."""
        return self.cap_controls_integer(2, 0)

    def write_mode(self, argument):
        """Sets the type of automatic controller (see manual for details)."""
        return self.cap_controls_integer(3, argument)

    def read_monitored_term(self):
        """Gets the terminal number on the element that PT and CT are connected to."""
        return self.cap_controls_integer(4, 0)

    def write_monitored_term(self, argument):
        """Sets the terminal number on the element that PT and CT are connected to."""
        return self.cap_controls_integer(5, argument)

    def read_use_volt_override(self):
        """Gets if Vmin and Vmax are enabled to override the control Mode."""
        return self.cap_controls_integer(6, 0)

    def write_use_volt_override(self, argument):
        """Sets if enables Vmin and Vmax to override the control Mode."""
        return self.cap_controls_integer(7, argument)

    def count(self):
        """Gets the number of CapControls in Active Circuit."""
        return self.cap_controls_integer(8, 0)
