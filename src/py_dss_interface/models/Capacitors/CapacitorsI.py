# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CapacitorsI(Base):

    def capacitors_integer(self, first, second):
        return int(self.dss_obj.CapacitorsI(ctypes.c_int32(first), ctypes.c_int32(second)))

    def read_nums_teps(self):
        """Gets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        return self.capacitors_integer(0, 0)

    # TODO: checar o parâmetro argument
    def write_num_steps(self, argument):
        """Sets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        return self.capacitors_integer(1, 0)

    def read_isdelta(self):
        """Gets 1 if delta connection, otherwise will return 0 for distributing and switching the total kvar."""
        return self.capacitors_integer(2, 0)

    def write_isdelta(self, argument):
        """Sets (Argument) 1 if delta connection, otherwise will return 0 for distributing and switching the total
        kvar. """
        return self.capacitors_integer(3, argument)

    def first(self):
        """Sets the first capacitor active. Returns 0 if no more."""
        return self.capacitors_integer(4, 0)

    def next(self):
        """Sets the next capacitor active. Returns 0 if no more."""
        return self.capacitors_integer(5, 0)

    def count(self):
        """Gets the number of capacitor objects in active circuit."""
        return self.capacitors_integer(6, 0)

    def add_step(self):
        """Adds one step of the capacitor if available. If successful returns 1."""
        return self.capacitors_integer(7, 0)

    def subtract_step(self):
        """Subtracts one step of the capacitor if available. If no more steps, returns 0."""
        return self.capacitors_integer(8, 0)

    def available_steps(self):
        """Gets the number of steps available in cap bank to be switched ON."""
        return self.capacitors_integer(9, 0)

    def open(self):
        """Opens all steps, all phases of the capacitor."""
        return self.capacitors_integer(10, 0)

    def close(self):
        """Closes all steps, all phases of the capacitor."""
        return self.capacitors_integer(11, 0)
