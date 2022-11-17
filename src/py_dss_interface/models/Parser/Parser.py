# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Parser.ParserF import ParserF
from py_dss_interface.models.Parser.ParserI import ParserI
from py_dss_interface.models.Parser.ParserS import ParserS
from py_dss_interface.models.Parser.ParserV import ParserV


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
        return ParserF._dbl_value(self)

    @property
    def int_value(self) -> int:
        return ParserI._int_value(self)

    @property
    def reset_delimiters(self) -> int:
        return ParserI._reset_delimiters(self)

    @property
    def auto_increment(self) -> int:
        return ParserI._auto_increment_read(self)

    @auto_increment.setter
    def auto_increment(self, argument: int):
        ParserI._auto_increment_write(self, argument)
