# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


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

    def ctrlqueue_ctrlqueue(self) -> str:
        """Delivers the control actions contained in the CtrlQueue after the latest solve command."""
        return Bridge.var_array_function(self.dss_obj.CtrlQueueV, 0, None, '')
