# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Meters import Meters
from py_dss_interface.models.Text.Text import Text
from typing import List


class MetersV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void MetersV(int32_t Parameter, VARIANT *arg);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    # TODO add type return

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 0)

    def _register_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 1)

    def _register_values(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 2)

    def _totals(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 3)

    def _peak_current_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 4)

    def _peak_current_write(self, arg: List[float]) -> List[float]:
        """Receives an array of doubles to set values of Peak Current Property."""
        return Bridge.variant_pointer_write(self._dss_obj.MetersV, 5, arg)

    def _calc_current_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 6)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/6
    # TODO include in test
    def _calc_current_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.MetersV, 7, arg)

    def _alloc_factors_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 8)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/7
    def _alloc_factors_write(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.MetersV, 9, arg)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/8
    def _all_end_elements(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 10)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/9
    # TODO include in test
    def _all_branches_in_zone(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 11)

    def _all_pce_in_zone(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MetersV, 12)
