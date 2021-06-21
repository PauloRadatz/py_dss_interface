# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.LineCodes import LineCodesS
from py_dss_interface.models.Text.Text import Text


class LineCodesV(Base):
    """
    This interface can be used to read/modify the properties of the LineCode Class where the values are Variants.

    The structure of the interface is as follows:
        void LineCodesV(int32_t Parameter, , VARIANT *Argument);

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def linecodes_read_rmatrix(self) -> str:
        """Gets the resistance matrix in ohms per unit length of the active LineCode."""
        return Bridge.var_array_function(self.dss_obj.LineCodesV, 0, None, '')

    def linecodes_write_rmatrix(self, argument: str) -> None:
        """Sets the resistance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument.
         :param argument: must be a string like that [0.791721 | 0.318476 0.781649 | 0.28345, 0.318476, 0.791721]
         """
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        lc = LineCodesS.LineCodesS(self.dss_obj)
        lc_name = lc.linecodes_read_name()
        t.text(f'edit Linecode.{lc_name} Rmatrix = {argument}')

    def linecodes_read_xmatrix(self) -> str:
        """Gets the reactance matrix in ohms per unit length of the active LineCode."""
        return Bridge.var_array_function(self.dss_obj.LineCodesV, 2, None, '')

    def linecodes_write_xmatrix(self, argument: str) -> None:
        """Sets the reactance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument.

         """
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        lc = LineCodesS.LineCodesS(self.dss_obj)
        lc_name = lc.linecodes_read_name()
        t.text(f'edit Linecode.{lc_name} Xmatrix = {argument}')

    def linecodes_read_cmatrix(self) -> str:
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        return Bridge.var_array_function(self.dss_obj.LineCodesV, 4, None, '')

    def linecodes_write_cmatrix(self, argument: str) -> None:
        """Sets the capacitance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument.
         :param argument: must be a string like that [383.948  |0  383.948  |0  0  383.948 ]
         """
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        lc = LineCodesS.LineCodesS(self.dss_obj)
        lc_name = lc.linecodes_read_name()
        t.text(f'edit Linecode.{lc_name} Cmatrix = {argument}')

    def linecodes_all_names(self) -> str:
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        return Bridge.var_array_function(self.dss_obj.LineCodesV, 6, None, '')
