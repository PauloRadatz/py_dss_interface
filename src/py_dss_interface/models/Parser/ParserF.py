# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Parser(Base):
    """
    This interface implements the CmathLib (ICmathLib) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface.
    """

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double ParserF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following. """
    def parser_dblvalue(self):
        """Returns next parameter as a double."""
        result = float(self.dss_obj.ParserF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    # ParserS (String)
    def parser_read_cmdstring(self):
        """Gets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_cmdstring(self, argument):
        """Sets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_nextparam(self):
        """Gets next token and return tag name (before = sign) if any. See Autoincrement."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_strvalue(self):
        """Returns next parameter as a string."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_read_whitespace(self):
        """Gets the characters used for White space in the command string. Default in blank and Tab."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_whitespace(self, argument):
        """Sets the characters used for White space in the command string. Default in blank and Tab."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_beginquote(self):
        """Gets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_beginquote(self, argument):
        """Sets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_endquote(self):
        """Gets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_endquote(self, argument):
        """Sets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_delimiters(self):
        """Gets the string defining hard delimiters used to separate token on the command string.
        Default is , and =. The = separates token name from token value. These override whitespaces to separate
        tokens."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_delimiters(self, argument):
        """Sets the string defining hard delimiters used to separate token on the command string.
        Default is , and =. The = separates token name from token value. These override whitespace to separate
        tokens."""
        result = ctypes.c_char_p(self.dss_obj.ParserS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    # ParserV (Variant)
    def parser_vector(self):
        """Returns token as variant array of doubles. For parsing quoted array syntax."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParserV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def parser_matrix(self):
        """Use this property to parse a Matrix token in OpenDSS format. Returns square matrix of order specified.
        Order same as default fortran order: column by column."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParserV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def parser_symmatrix(self):
        """Use this property to parse a Matrix token in lower triangular form. Symmetry is forced."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParserV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
