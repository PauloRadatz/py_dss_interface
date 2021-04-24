# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CircuitF(Base):

    def circuit_float(self, first, second):
        return float(self.dss_obj.CircuitF(ctypes.c_int32(first), ctypes.c_double(second)))

    def circuit_capacity(self):
        """Returns the total capacity of the active circuit.
        Or this parameter it is necessary to specify the start and increment of the capacity in the arguments argument1
        and argument2 respectively."""
        return self.circuit_float(0, 0)
