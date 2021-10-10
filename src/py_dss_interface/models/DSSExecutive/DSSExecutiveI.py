# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSExecutiveI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t DSSExecutiveI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def executive_num_commands(self) -> int:
        """Gets the number of DSS Executive Commands."""
        return int(self.dss_obj.DSSExecutiveI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def executive_num_options(self) -> int:
        """Gets the number of DSS Executive Options."""
        return int(self.dss_obj.DSSExecutiveI(ctypes.c_int32(1), ctypes.c_int32(0)))
