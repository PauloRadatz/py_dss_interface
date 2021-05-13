# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Relays(Base):
    """
    This interface implements the Relays (IRelays) interface of OpenDSS by declaring 3 procedures for accessing the
    different properties included in this interface: .
    """

    # RelaysS (String)
    def relays_read_name(self):
        """Gets the name of the active Relay."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_name(self, argument):
        """Sets the name of the active Relay."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def relays_read_monitoredobj(self):
        """Gets the full name of the object this relay is monitoring."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_monitoredobj(self, argument):
        """Sets the full name of the object this relay is monitoring."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def relays_read_switchedobj(self):
        """Gets the full name of element that will switched when relay trips."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_switchedobj(self, argument):
        """Sets the full name of element that will switched when relay trips."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # RelaysV (Variant)
    def relays_allnames(self):
        """Getsa variant array of strings containing names of all relay elements."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.RelaysV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
