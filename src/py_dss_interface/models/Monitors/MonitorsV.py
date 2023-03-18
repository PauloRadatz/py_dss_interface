# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
import numpy as np
from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List

class MonitorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.
    The structure of the interface is as follows:
        void MonitorsV(int32_t Parameter, VARIANT *Argument);
    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MonitorsV, 0)

    def _byte_stream(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.MonitorsV, 1)

    def _header(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.MonitorsV, 2)

    def _dbl_hour(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MonitorsV, 3)

    def _dbl_freq(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MonitorsV, 4)

    def _channel(self, arg: int) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.MonitorsV, 5, arg)

