# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class DSSInterfaceV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void DSSV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def dss_classes(self) -> List[str]:
        """Gets the list of DSS intrinsic classes (names of the classes)."""
        return Bridge.var_array_function(self.dss_obj.DSSV, 0, None, '')

    def dss_user_classes(self) -> List[str]:
        """Gets list of user-defined classes (names of the classes)."""
        return Bridge.var_array_function(self.dss_obj.DSSV, 1, None, '')
