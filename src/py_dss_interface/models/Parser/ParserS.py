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

    def parser_read_cmd_string(self):
        """Gets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_cmd_string(self, argument):
        """Sets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_next_param(self):
        """Gets next token and return tag name (before = sign) if any. See Autoincrement."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_str_value(self):
        """Returns next parameter as a string."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_read_white_space(self):
        """Gets the characters used for White space in the command string. Default in blank and Tab."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_white_space(self, argument):
        """Sets the characters used for White space in the command string. Default in blank and Tab."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_begin_quote(self):
        """Gets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_begin_quote(self, argument):
        """Sets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_end_quote(self):
        """Gets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_end_quote(self, argument):
        """Sets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_delimiters(self):
        """Gets the string defining hard delimiters used to separate token on the command string. Default is ,
        and =. The = separates token name from token value. These override whitespaces to separate tokens. """
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_delimiters(self, argument):
        """Sets the string defining hard delimiters used to separate token on the command string. Default is ,
        and =. The = separates token name from token value. These override whitespace to separate tokens. """
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')
