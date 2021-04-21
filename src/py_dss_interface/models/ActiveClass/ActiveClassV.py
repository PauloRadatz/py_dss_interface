# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class ActiveClassV(Base):
    def active_class_variant(self, first, second):
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ActiveClassV(ctypes.c_int32(first), variant_pointer)
        return variant_pointer.contents.value

    def all_names(self):
        """Gets a variant array of strings consisting of all element names in the active Class."""
        return self.active_class_variant(0)
