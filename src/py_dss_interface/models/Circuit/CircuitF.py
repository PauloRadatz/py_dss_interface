# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CircuitF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double CircuitF(int32_t Parameter, double Argument1, double Argument2);

    This interface returns a floating point number (IEEE754 64 bits) according to the number sent in the variable
    “parameter”. The parameter can be one of the following.
    """

    def _capacity(self, capacity_start: float, capacity_increment: float) -> float:
        return float(self._dss_obj.CircuitF(ctypes.c_int32(0), ctypes.c_double(capacity_start), ctypes.c_double(capacity_increment)))
