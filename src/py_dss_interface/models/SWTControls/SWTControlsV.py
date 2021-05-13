# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class SWTControls(Base):
    """
    This interface implements the SwtControls (ISwtControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: .
    """

    # SwtControlsV (Variant)
    def swtcontrols_allnames(self):
        """Gets a variant array of strings with all SwtControl names in the active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SwtControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
