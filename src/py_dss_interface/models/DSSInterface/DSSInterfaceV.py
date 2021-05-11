# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from typing import List

from comtypes import automation
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
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value

    def dss_user_classes(self) -> List[str]:
        """Gets list of user-defined classes (names of the classes)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSV(ctypes.c_int32(1), variant_pointer)
        return variant_pointer.contents.value
