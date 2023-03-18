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

    def _pct_progress(self, arg: float) -> int:
        return int(self._dss_obj.DSSProgressI(ctypes.c_int32(0), ctypes.c_float(arg)))

    def _show(self) -> int:
        return int(self._dss_obj.DSSProgressI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def _close(self) -> int:
        return int(self._dss_obj.DSSProgressI(ctypes.c_int32(2), ctypes.c_int32(0)))
