# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CapControlsS(Base):

    def cap_controls_string(self, first, second):
        result = ctypes.c_char_p(self.dss_obj.CapControlsS(ctypes.c_int32(first), ctypes.c_int32(second)))
        return result.value.decode('ascii')

    def cap_controls_read_name(self):
        """Gets the name of the active CapControl."""
        return self.cap_controls_string(0, 0)

    def cap_controls_write_name(self, argument):
        """Sets a CapControl active by name."""
        return self.cap_controls_string(1, argument.encode('ascii'))

    def cap_controls_read_capacitor(self):
        """Gets the name of the capacitor that is controlled."""
        return self.cap_controls_string(2, 0)

    def cap_controls_write_capacitor(self, argument):
        """Sets the name of the capacitor that is controlled."""
        return self.cap_controls_string(3, argument.encode('ascii'))

    def cap_controls_read_monitored_obj(self):
        """Gets the full name of the element that PT and CT are connected to."""
        return self.cap_controls_string(4, 0)

    def cap_controls_write_monitored_obj(self, argument):
        """Sets the full name of the element that PT and CT are connected to."""
        return self.cap_controls_string(5, argument.encode('ascii'))
