# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class CapacitorsV(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are Variants.

    The structure of the interface is as follows:
        void CapacitorsV(int32_t Parameter, VARIANT *Argument)

    This interface returns a Variant, the first parameter is used to specify the property of the class to be used and
    the second parameter can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CapacitorsV, 0)

    def _states_read(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.CapacitorsV, 1)

    def _states_write(self, argument: List[int]) -> List[int]:
        return Bridge.variant_pointer_write(self._dss_obj.CapacitorsV, 2, argument)

