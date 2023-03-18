# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class ActiveClassV(Base):
    """
    This interface can be used to read/modify the properties of the ActiveClass Class where the values are Variants.

    The structure of the interface is as follows:
        void ActiveClassI(int32_t Parameter, VARIANT *Argument)

    This interface returns a Variant, the first parameter is used to specify the property of the class to be used and
    the second parameter can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _names(self) -> List[str]:
        try:
            return Bridge.variant_pointer_read(self._dss_obj.ActiveClassV, 0)
        except Exception as e:
            raise Exception(f"Error while retrieving element names: {e}")
