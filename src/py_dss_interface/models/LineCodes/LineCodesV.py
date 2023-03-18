# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.LineCodes import LineCodes
from py_dss_interface.models.Text.Text import Text
from typing import List


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

    # TODO I do not think they should return str

    def _rmatrix_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LineCodesV, 0)

    def _rmatrix_write(self, argument: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.LineCodesV, 1, argument)

    def _xmatrix_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LineCodesV, 2)

    def _xmatrix_write(self, argument: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.LineCodesV, 3, argument)

    def _cmatrix_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LineCodesV, 4)

    def _cmatrix_write(self, argument: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.LineCodesV, 5, argument)

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.LineCodesV, 6)
