# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.LoadShapes import LoadShapes
from py_dss_interface.models.Text.Text import Text
from typing import List


class LoadShapesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void LoadShapeV(int32_t Parameter, VARIANT *arg);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following. """

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.LoadShapeV, 0)

    def _p_mult(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LoadShapeV, 1)

    def _p_mult_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.LoadShapeV, 2, arg)

    def _q_mult(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LoadShapeV, 3)

    def _q_mult_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.LoadShapeV, 4, arg)

    def _time_array(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.LoadShapeV, 5)

    def _time_array_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.LoadShapeV, 6, arg)
