# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from typing import List

from comtypes import automation
from py_dss_interface.models.Base import Base


class GeneratorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void GeneratorsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant as a result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def generators_allnames(self) -> List[str]:
        """Gets the array of names of all Generator objects."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.GeneratorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def generators_registernames(self) -> List[str]:
        """Gets the array of names of all generator Energy Meter registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.GeneratorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def generators_registervalues(self) -> List[float]:
        """Gets the array of values in generator Energy Meter registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.GeneratorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
