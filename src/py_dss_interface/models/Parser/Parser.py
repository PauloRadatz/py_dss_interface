# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Parser.ParserF import ParserF
from py_dss_interface.models.Parser.ParserI import ParserI
from py_dss_interface.models.Parser.ParserS import ParserS
from py_dss_interface.models.Parser.ParserV import ParserV
from typing import List


class Parser(ParserI, ParserS, ParserF, ParserV):
    """
    This interface implements the CmathLib (ICmathLib) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ParserI, ParserS, ParserF, ParserV.
    """

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ParserV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def dbl_value(self) -> float:
        """Returns next parameter as a double."""
        return ParserF._dbl_value(self)

    @property
    def int_value(self) -> int:
        """Returns next parameter as a long integer."""
        return ParserI._int_value(self)

    @property
    def reset_delimiters(self) -> int:
        """Reset delimiters to their default values."""
        return ParserI._reset_delimiters(self)

    @property
    def auto_increment(self) -> int:
        """In this parameter the default is false (0). If true (1) parser automatically advances to next token after
                 DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.

        In this parameter the default is false (0). If true (1) parser automatically advances to next token after
                 DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names."""
        return ParserI._auto_increment_read(self)

    @auto_increment.setter
    def auto_increment(self, argument: int):
        ParserI._auto_increment_write(self, argument)

    @property
    def cmd_string(self) -> str:
        """Gets a string to be parsed. Loading this string resets the parser to the beginning of the line.
                Then parse off the tokens in sequence.

        Sets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        return ParserS._cmd_string_read(self)

    @cmd_string.setter
    def cmd_string(self, arg: str):
        ParserS._cmd_string_write(self, arg)

    @property
    def parser_next_param(self) -> str:
        """Gets next token and return tag name (before = sign) if any. See Autoincrement."""
        return ParserS._parser_next_param(self)

    @property
    def parser_str_value(self) -> str:
        """Returns next parameter as a string."""
        return ParserS._parser_str_value(self)

    @property
    def white_space(self) -> str:
        """Gets the characters used for White space in the command string. Default in blank and Tab.

        Sets the characters used for White space in the command string. Default in blank and Tab."""
        return ParserS._white_space_read(self)

    @white_space.setter
    def white_space(self, arg: str):
        ParserS._white_space_write(self, arg)

    @property
    def begin_quote(self) -> str:
        """Gets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
                 Default is "([{.

        Sets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        return ParserS._begin_quote_read(self)

    @begin_quote.setter
    def begin_quote(self, arg: str):
        ParserS._begin_quote_write(self, arg)

    @property
    def end_quote(self) -> str:
        """Gets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
                 Default is ")]}.

        Sets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        return ParserS._end_quote_read(self)

    @end_quote.setter
    def end_quote(self, arg: str):
        ParserS._end_quote_write(self, arg)

    @property
    def delimiters(self) -> str:
        """Gets the string defining hard delimiters used to separate token on the command string. Default is ,
                and =. The = separates token name from token value. These override whitespaces to separate tokens.

        Sets the string defining hard delimiters used to separate token on the command string. Default is ,
        and =. The = separates token name from token value. These override whitespace to separate tokens. """
        return ParserS._delimiters_read(self)

    @delimiters.setter
    def delimiters(self, arg: str):
        ParserS._delimiters_write(self, arg)

    @property
    def vector(self) -> List[float]:
        """Returns token as variant array of doubles. For parsing quoted array syntax."""
        return ParserV._vector(self)

    @property
    def matrix(self) -> List[float]:
        """Use this property to parse a Matrix token in OpenDSS format. Returns square matrix of order specified.
                Order same as default fortran order: column by column. """
        return ParserV._matrix(self)

    @property
    def sym_matrix(self) -> List[float]:
        """Use this property to parse a Matrix token in lower triangular form. Symmetry is forced."""
        return ParserV._sym_matrix(self)


