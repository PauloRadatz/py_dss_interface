# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Text.Text import Text
from typing import List


class SensorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SensorsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.SensorsV, 0)

    def _currents_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.SensorsV, 1)

    def _currents_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.SensorsV, 2, arg)

    def _kvars_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.SensorsV, 3)

    def _kvars_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.SensorsV, 4, arg)

    def _kws_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.SensorsV, 5)

    def _kws_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.SensorsV, 6, arg)
