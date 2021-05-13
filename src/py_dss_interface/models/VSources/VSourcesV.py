# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class VSourcesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void VSourcesV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def vsources_allnames(self):
        """Gets the name of the active VSource."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.VsourcesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
