# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Text.Text import Text
from py_dss_interface.models.XYCurves import XYCurves
from typing import List


class XYCurvesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void XYCurvesS(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _x_array_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.XYCurvesV, 0)

    def _x_array_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.XYCurvesV, 1, arg)

    def _y_array_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.XYCurvesV, 2)

    def _y_array_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.XYCurvesV, 3, arg)
