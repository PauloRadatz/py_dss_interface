# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class VSources(Base):
    """
    This interface implements the Vsources (IVSources) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """


    # VSourcesV (Variant)
    def vsources_allnames(self):
        """Gets the name of the active VSource."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.VsourcesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
