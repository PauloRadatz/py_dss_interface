# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class Settings(Base):
    """
    This interface implements the Settings (ISettings) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    # SettingsS (String)
    def settings_read_autobuslist(self):
        """Gets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def settings_write_autobuslist(self, argument):
        """Sets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def settings_read_pricecurve(self):
        """Gets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def settings_write_pricecurve(self, argument):
        """Sets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SettingsV (Variant)
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
