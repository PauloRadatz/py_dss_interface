# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List

class TransformersV(Base):
    """
    This interface can be used to read/modify the properties of the Transformers Class where the values are Variants.

    The structure of the interface is as follows:
        void TransformersV(int32_t Parameter, VARIANT *Argument) ;

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.TransformersV, 0)

    def _wdg_voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.TransformersV, 1)

    def _wdg_currents(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.TransformersV, 2)
