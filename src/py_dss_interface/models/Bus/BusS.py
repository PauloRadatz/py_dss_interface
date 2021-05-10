# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class BusS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object. The structure of the
    interface is as follows:
        CStr BUSS(int32_t Parameter, CStr Argument)

    This interface returns a string according to the number sent in the first parameter. That parameter is an string
    and could be call by the theses methods below.
    """

    def bus_name(self):
        """Returns the name of the active bus."""
        return self.get_string(0, 0)
