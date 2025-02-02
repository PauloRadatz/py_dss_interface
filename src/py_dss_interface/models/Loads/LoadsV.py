# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List


class LoadsV(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are variants (the
    value can have different formats).

    The structure of the interface is as follows:
        void DSSLoadsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a string, the variable “parameter” (Integer) is used to specify the property of the class
    to be used and the variable “argument” (Variant) is used to return the variant structure.
    """

    def _names(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.DSSLoadsV, 0)

    def _zipv_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.DSSLoadsV, 1)

    def _zipv_write(self, argument: List[float]) -> List[float]:
        return Bridge.pointer_write(self._dss_obj.DSSLoadsV, 2, argument, 2)
