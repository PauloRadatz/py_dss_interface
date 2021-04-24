# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapacitorsS(Base):

    def capacitors_read_name(self):
        """Gets the name of the active Capacitor element."""
        return self.get_string(0, 0)

    def capacitors_write_name(self, argument):
        """Sets the name of the Capacitor element to set it active."""
        return self.get_string(1, 0)
