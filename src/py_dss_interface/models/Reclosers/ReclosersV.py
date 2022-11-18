# -*- coding: iso-8859-15 -*-

import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List


class ReclosersV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ReclosersV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _names(self) -> List[str]:
        """Gets a variant array of strings with names of all reclosers in active circuit."""
        return Bridge.var_array_function(self.dss_obj.ReclosersV, ctypes.c_int(0), ctypes.c_int(0), None)

    def _reclose_intervals(self) -> List[float]:
        """Gets a variant array of doubles: reclose intervals (s) between shots."""
        return Bridge.var_array_function(self.dss_obj.ReclosersV, ctypes.c_int(1), ctypes.c_int(0), None)
