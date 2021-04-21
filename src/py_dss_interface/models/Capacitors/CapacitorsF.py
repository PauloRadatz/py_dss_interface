# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CapacitorsF(Base):

    def capacitors__float(self, first, second):
        return float(self.dss_obj.CapacitorsF(ctypes.c_int32(first), ctypes.c_double(second)))

    def read_kv(self):
        """Gets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        return self.capacitors__float(0, 0)

    def write_kv(self, argument):
        """Sets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        return self.capacitors__float(1, argument)

    def read_kvar(self):
        """Gets the total bank kvar, distributed equally among phases and steps."""
        return self.capacitors__float(2, 0)

    def write_kvar(self, argument):
        """Sets the total bank kvar, distributed equally among phases and steps."""
        return self.capacitors__float(3, argument)
