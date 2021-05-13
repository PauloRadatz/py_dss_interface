# -*- coding: iso-8859-15 -*-

import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class Reclosers(Base):
    """
    This interface implements the Reclosers (IReclosers) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    # ReclosersV (Variant)
    def reclosers_allnames(self):
        """Gets a variant array of strings with names of all reclosers in active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ReclosersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def reclosers_recloseintervals(self):
        """Gets a variant array of doubles: reclose intervals (s) between shots."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ReclosersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value
