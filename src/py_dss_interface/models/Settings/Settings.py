# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Settings(Base):

    # SettingsI (int)
    def settings_read_allowduplicates(self):
        """Gets if OpenDSS allows duplicate names of objects: {1 allow, 0 not allow}."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def settings_write_allowduplicates(self, argument):
        """Sets if OpenDSS allows duplicate names of objects: {1 allow, 0 not allow}."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(1), ctypes.c_int32(argument))
        return result

    def settings_read_zonelock(self):
        """Gets the status of Lock zones on energy meters to prevent rebuilding if a circuit
        change occurs: {1= true, 0= False}."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def settings_write_zonelock(self, argument):
        """Sets the status of Lock zones on energy meters to prevent rebuilding if a circuit
        change occurs: {1= true, 0= False}."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def settings_read_cktmodel(self):
        """Gets {dssMultiphase* | dssPositiveSeq} Indicate if the circuit model is positive sequence."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def settings_write_cktmodel(self, argument):
        """Sets {dssMultiphase* | dssPositiveSeq} Indicate if the circuit model is positive sequence."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def settings_read_trapezoidal(self):
        """Gets {True (1) | False (0)} value of trapezoidal integration flag in Energy Meters."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def settings_write_trapezoidal(self, argument):
        """Sets {True (1) | False (0)} value of trapezoidal integration flag in Energy Meters."""
        result = self.dss_obj.SettingsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    # SettingsF (Float)
    def settings_allocationfactors(self):
        """Sets all load allocation factors for all loads defined by XFKVA property to this value."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def settings_read_normvminpu(self):
        """Gets the per unit minimum voltage for Normal conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    def settings_write_normvminpu(self, argument):
        """Sets the per unit minimum voltage for Normal conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(2), ctypes.c_double(argument)))
        return result

    def settings_read_normvmaxpu(self):
        """Gets the per unit maximum voltage for Normal conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def settings_write_normvmaxpu(self, argument):
        """Sets the per unit maximum voltage for Normal conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(4), ctypes.c_double(argument)))
        return result

    def settings_read_emergvminpu(self):
        """Gets the per unit minimum voltage for Emergency conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def settings_write_emergvminpu(self, argument):
        """Sets the per unit minimum voltage for Emergency conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(6), ctypes.c_double(argument)))
        return result

    def settings_read_emergvmaxpu(self):
        """Gets the per unit maximum voltage for Emergency conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    def settings_write_emergvmaxpu(self, argument):
        """Sets the per unit maximum voltage for Emergency conditions."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(8), ctypes.c_double(argument)))
        return result

    def settings_read_ueweight(self):
        """Gets the weighting factor applied to UE register values."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(9), ctypes.c_double(0)))
        return result

    def settings_write_ueweight(self, argument):
        """Sets the weighting factor applied to UE register values."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(10), ctypes.c_double(argument)))
        return result

    def settings_read_lossweight(self):
        """Gets the weighting factor applied to Loss register values."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(11), ctypes.c_double(0)))
        return result

    def settings_write_lossweight(self, argument):
        """Sets the weighting factor applied to Loss register values."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(12), ctypes.c_double(argument)))
        return result

    def settings_read_pricesignal(self):
        """Gets the price signal for the circuit."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(13), ctypes.c_double(0)))
        return result

    def settings_write_pricesignal(self, argument):
        """Sets the price signal for the circuit."""
        result = float(self.dss_obj.SettingsF(ctypes.c_int32(14), ctypes.c_double(argument)))
        return result

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
