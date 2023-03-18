# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List

class CtrlQueueV(Base):
    """
    This interface can be used to read/modify the properties of the CtrlQueue Class where the values are Variants.

    The structure of the interface is as follows:
        void CtrlQueueV(int32_t Parameter, , VARIANT *Argument);

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _ctrlqueue(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CtrlQueueV, 0)

    def _push(self, arg: List[float]) -> List[float]:
        return Bridge.variant_pointer_write(self._dss_obj.CtrlQueueV, 1, arg)
