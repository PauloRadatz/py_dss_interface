# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class Meters(Base):
    """
    This interface implements the Meters (IMeters) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: .
    """

    # MetersV (Variant)
    def meters_allnames(self):
        """Returns an array of all Energy Meter names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def meters_registernames(self):
        """Returns an array of strings containing the names of the registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def meters_registervalues(self):
        """Returns an array of values contained in the Meter registers for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def meters_totals(self):
        """Returns the totals for all registers of all Meters."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_peakcurrent(self):
        """Returns an array of doubles with the Peak Current Property."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_peakcurrent(self, argument):
        """Receives an array of doubles to set values of Peak Current Property."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MetersV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_calcurrent(self):
        """Returns the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_calcurrent(self, argument):
        """Sets the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MetersV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_allocfactors(self):
        """Returns an array of doubles: allocation factors for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_allocfactors(self, argument):
        """Receives an array of doubles to set the phase allocation factors for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MetersV(ctypes.c_int(9), variant_pointer)
        return variant_pointer.contents.value

    def meters_allendelements(self):
        """Returns a variant array of names of all zone end elements."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(10), variant_pointer)
        return variant_pointer.contents.value

    def meters_allbranchesinzone(self):
        """Returns a wide string list of all branches in zone of the active Energy Meter object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(11), variant_pointer)
        return variant_pointer.contents.value
