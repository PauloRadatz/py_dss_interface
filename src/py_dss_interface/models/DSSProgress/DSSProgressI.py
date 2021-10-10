# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSProgressI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t DSSProgressI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def dssprogress_pct_progress(self, arg: float) -> int:
        """Sets the percent progress to indicate [0..100]."""
        return int(self.dss_obj.DSSProgressI(ctypes.c_int32(0), ctypes.c_float(arg)))

    def dssprogress_show(self) -> int:
        """Shows progress form with null caption and progress set to zero."""
        return int(self.dss_obj.DSSProgressI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def dssprogress_close(self) -> int:
        """Closes (hides) DSS Progress form."""
        return int(self.dss_obj.DSSProgressI(ctypes.c_int32(2), ctypes.c_int32(0)))
