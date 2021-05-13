# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class SettingsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SettingsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def settings_read_ueregs(self):
        """Gets the array of Integers defining Energy Meter registers to use for computing UE."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SettingsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def settings_write_ueregs(self, argument):
        """Sets the array of Integers defining Energy Meter registers to use for computing UE."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.SettingsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def settings_read_lossregs(self):
        """Gets the array of Integers defining Energy Meter registers to use for computing Losses."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SettingsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def settings_write_lossregs(self, argument):
        """Sets the array of Integers defining Energy Meter registers to use for computing Losses."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.SettingsV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def settings_read_voltagebases(self):
        """Gets the array of doubles defining the legal voltage bases in kV L-L."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SettingsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def settings_write_voltagebases(self, argument):
        """Sets the array of doubles defining the legal voltage bases in kV L-L."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.SettingsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value
