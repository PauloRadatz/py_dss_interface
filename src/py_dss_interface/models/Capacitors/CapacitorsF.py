# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapacitorsF(Base):

    def capacitors_read_kv(self):
        """Gets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        return self.get_float(0, 0)

    def capacitors_write_kv(self, argument):
        """Sets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        return self.get_float(1, argument)

    def capacitors_read_kvar(self):
        """Gets the total bank kvar, distributed equally among phases and steps."""
        return self.get_float(2, 0)

    def capacitors_write_kvar(self, argument):
        """Sets the total bank kvar, distributed equally among phases and steps."""
        return self.get_float(3, argument)
