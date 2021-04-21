# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class BusS(Base):

    def bus_string(self, first, second):
        result = ctypes.c_char_p(self.dss_obj.BUSS(ctypes.c_int32(first), ctypes.c_int32(second)))
        return result.value.decode('ascii')

    def name(self):
        """Returns the name of the active bus."""
        return self.bus_string(0, 0)
