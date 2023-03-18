# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class ParserF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double ParserF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following. """

    # TODO include in test
    def _dbl_value(self) -> float:
        return float(self._dss_obj.ParserF(ctypes.c_int32(0), ctypes.c_double(0)))
