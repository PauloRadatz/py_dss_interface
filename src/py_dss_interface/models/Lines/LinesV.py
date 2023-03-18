# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Lines import Lines
from py_dss_interface.models.Text.Text import Text
from py_dss_interface.utils.Error import Error


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

    # TODO work on this guy

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.LinesV, 0)

    def _rmatrix_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LinesV, 1)

    def _rmatrix_write(self, argument: List[float]) -> List[float]:
        # return Bridge.variant_pointer_write(self.dss_obj.LinesV, 2, argument)

        Error.method_not_working("setter of dss.lines.rmatrix")

    def _xmatrix_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LinesV, 3)

    def _xmatrix_write(self, argument: List[float]) -> List[float]:
        # return Bridge.variant_pointer_write(self.dss_obj.LinesV, 4, argument)

        Error.method_not_working("setter of dss.lines.xmatrix")

    # TODO include in test
    def _cmatrix_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LinesV, 5)

    # TODO include in test
    def _cmatrix_write(self, argument: List[float]) -> List[float]:
        # return Bridge.variant_pointer_write(self.dss_obj.LinesV, 6, argument)

        Error.method_not_working("setter of dss.lines.cmatrix")

    def _yprim_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LinesV, 7)

    # TODO include in test
    def _yprim_write(self, argument: List[float]) -> List[float]:
        return "According the official documentation this parameter does nothing at present."
        # Below we can see a possible future implementation
        # argument = Base.check_string_param(argument)
        # t = Text(self.dss_obj)
        # lc = Lines.LinesS(self.dss_obj)
        # lc_name = lc.lines_read_name()
        # return t.text(f'edit Line.{lc_name} Yprim = {argument}')
