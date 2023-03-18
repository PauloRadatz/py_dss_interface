# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class ParserI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t ParserI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    # TODO include in test
    def _int_value(self) -> int:
        return self._dss_obj.ParserI(ctypes.c_int32(0), ctypes.c_int32(0))

    # TODO include in test
    def _reset_delimiters(self) -> int:
        return self._dss_obj.ParserI(ctypes.c_int32(1), ctypes.c_int32(0))

    # TODO include in test
    def _auto_increment_read(self) -> int:
        return self._dss_obj.ParserI(ctypes.c_int32(2), ctypes.c_int32(0))

    # TODO include in test
    def _auto_increment_write(self, argument: int) -> int:
        return self._dss_obj.ParserI(ctypes.c_int32(3), ctypes.c_int32(argument))
