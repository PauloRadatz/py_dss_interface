# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CapControlsV(Base):

    def cap_controls_variant(self, first):
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CapControlsV(ctypes.c_int32(first), variant_pointer)
        return variant_pointer.contents.value

    def all_names(self):
        return self.cap_controls_variant(0)
