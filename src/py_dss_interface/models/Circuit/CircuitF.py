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

    # TODO include in test. What is it?
    def _circuit_float(self, first, second, third) -> float:
        return float(self._dss_obj.CircuitF(ctypes.c_int32(first), ctypes.c_double(second), ctypes.c_double(third)))

    def _capacity(self, capacity_start=0, capacity_increment=0.1) -> float:
        return self._circuit_float(0, capacity_start, capacity_increment)
