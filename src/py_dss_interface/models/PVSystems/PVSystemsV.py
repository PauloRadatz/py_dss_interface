# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class PVSystemsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void PVSystemsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def pvsystems_allnames(self):
        """Gets the variant array of string containing names of all PVSystems in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.PVsystemsV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value
