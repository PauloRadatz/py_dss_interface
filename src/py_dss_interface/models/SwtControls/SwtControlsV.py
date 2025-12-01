# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List


class SwtControlsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SwtControlsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _names(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.SwtControlsV, 0)

    def _state_read(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.SwtControlsV, 1)

    def _state_write(self, argument:  List[str]):
        return Bridge.pointer_write(self._dss_obj.SwtControlsV, 2, argument,4)

    def _normal_state_read(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.SwtControlsV, 3)

    def _normal_state_write(self, argument:  List[str]):
        return Bridge.pointer_write(self._dss_obj.SwtControlsV, 4, argument,4)
