# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Lines import Lines
from py_dss_interface.models.Text.Text import Text


class LinesV(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are Variants.

    The structure of the interface is as follows:
        void LinesV(int32_t Parameter, , VARIANT *Argument);

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.

    """

    def lines_allnames(self) -> List[str]:
        """Gets the name of all Line Objects."""
        return Bridge.VarArrayFunction(self.dss_obj.LinesV, 0, None, '')

    def lines_read_rmatrix(self):
        """Gets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        return Bridge.VarArrayFunction(self.dss_obj.LinesV, 1, None, '')

    def lines_write_rmatrix(self, argument) -> int:
        """Sets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        lc = Lines.LinesS(self.dss_obj)
        lc_name = lc.lines_read_name()
        return t.text(f'edit Line.{lc_name} Rmatrix = {argument}')

    def lines_read_xmatrix(self) -> List[float]:
        """Gets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        return Bridge.VarArrayFunction(self.dss_obj.LinesV, 3, None, '')

    def lines_write_xmatrix(self, argument) -> List[float]:
        """Sets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        lc = Lines.LinesS(self.dss_obj)
        lc_name = lc.lines_read_name()
        return t.text(f'edit Line.{lc_name} Xmatrix = {argument}')

    def lines_read_cmatrix(self) -> List[float]:
        """Gets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        return Bridge.VarArrayFunction(self.dss_obj.LinesV, 5, None, '')

    def lines_write_cmatrix(self, argument) -> int:
        """Sets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        lc = Lines.LinesS(self.dss_obj)
        lc_name = lc.lines_read_name()
        return t.text(f'edit Line.{lc_name} Cmatrix = {argument}')

    def lines_read_yprim(self) -> List[float]:
        """Gets the YPrimitive of the active Line."""
        return Bridge.VarArrayFunction(self.dss_obj.LinesV, 7, None, '')

    # # TODO: check later
    # def lines_write_yprim(self, argument) -> List[float]:
    #     """Does nothing at present."""
    #     variant_pointer = ctypes.pointer(automation.VARIANT())
    #     variant_pointer.contents.value = argument
    #     self.dss_obj.LinesV(ctypes.c_int(8), variant_pointer)
    #     return variant_pointer.contents.value
