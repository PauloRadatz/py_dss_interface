# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class ParserS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr ParserS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    # TODO include in test
    # TODO ALLLLL

    def _cmd_string_read(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _cmd_string_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _parser_next_param(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _parser_str_value(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _white_space_read(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _white_space_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _begin_quote_read(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _begin_quote_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _end_quote_read(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _end_quote_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _delimiters_read(self):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _delimiters_write(self, argument):
        result = ctypes.c_char_p(self._dss_obj.ParserS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')
