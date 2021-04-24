# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class BusS(Base):

    def bus_name(self):
        """Returns the name of the active bus."""
        return self.get_string(0, 0)
