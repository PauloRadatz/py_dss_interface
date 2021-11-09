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
    def parser_int_value(self) -> int:
        """Returns next parameter as a long integer."""
        return self.dss_obj.ParserI(ctypes.c_int32(0), ctypes.c_int32(0))

    # TODO include in test
    def parser_reset_delimeters(self) -> int:
        """Reset delimiters to their default values."""
        return self.dss_obj.ParserI(ctypes.c_int32(1), ctypes.c_int32(0))

    # TODO include in test
    def parser_read_auto_increment(self) -> int:
        """In this parameter the default is false (0). If true (1) parser automatically advances to next token after
         DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names."""
        return self.dss_obj.ParserI(ctypes.c_int32(2), ctypes.c_int32(0))

    # TODO include in test
    def parser_write_auto_increment(self, argument) -> int:
        """In this parameter the default is false (0). If true (1) parser automatically advances to next token after
         DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names."""
        return self.dss_obj.ParserI(ctypes.c_int32(3), ctypes.c_int32(argument))
