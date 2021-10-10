# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class CapControlsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CapControlsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def capcontrols_all_names(self) -> List[str]:
        result = Bridge.var_array_function(self.dss_obj.CapControlsV, 0, None, '')
        if result == -1:
            raise ValueError("An error ocurred when tries to READ CapControls states! ")
        return result
