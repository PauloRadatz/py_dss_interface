# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CapacitorsS(Base):

    def capacitors_string(self, first, second):
        result = ctypes.c_char_p(self.dss_obj.BUSS(ctypes.c_int32(first), ctypes.c_int32(second)))
        return result.value.decode('ascii')

    def capacitors_read_name(self):
        """Gets the name of the active Capacitor element."""
        return self.capacitors_string(0, 0)

    def capacitors_write_name(self, argument):
        """Sets the name of the Capacitor element to set it active."""
        return self.capacitors_string(1, 0)
