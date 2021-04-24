# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapacitorsI(Base):

    def capacitors_read_nums_teps(self):
        """Gets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        return self.get_integer(0, 0)

    # TODO: checar o parâmetro argument
    def capacitors_write_num_steps(self, argument):
        """Sets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        return self.get_integer(1, 0)

    def capacitors_read_isdelta(self):
        """Gets 1 if delta connection, otherwise will return 0 for distributing and switching the total kvar."""
        return self.get_integer(2, 0)

    def capacitors_write_isdelta(self, argument):
        """Sets (Argument) 1 if delta connection, otherwise will return 0 for distributing and switching the total
        kvar. """
        return self.get_integer(3, argument)

    def capacitors_first(self):
        """Sets the first capacitor active. Returns 0 if no more."""
        return self.get_integer(4, 0)

    def capacitors_next(self):
        """Sets the next capacitor active. Returns 0 if no more."""
        return self.get_integer(5, 0)

    def capacitors_count(self):
        """Gets the number of capacitor objects in active circuit."""
        return self.get_integer(6, 0)

    def capacitors_add_step(self):
        """Adds one step of the capacitor if available. If successful returns 1."""
        return self.get_integer(7, 0)

    def capacitors_subtract_step(self):
        """Subtracts one step of the capacitor if available. If no more steps, returns 0."""
        return self.get_integer(8, 0)

    def capacitors_available_steps(self):
        """Gets the number of steps available in cap bank to be switched ON."""
        return self.get_integer(9, 0)

    def capacitors_open(self):
        """Opens all steps, all phases of the capacitor."""
        return self.get_integer(10, 0)

    def capacitors_close(self):
        """Closes all steps, all phases of the capacitor."""
        return self.get_integer(11, 0)
