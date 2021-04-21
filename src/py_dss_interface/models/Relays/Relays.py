# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Relays(Base):
    # RelaysI (int)
    def relays_count(self):
        """Gets number of Relays in active circuit."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def relays_first(self):
        """Sets first relay active. If none, returns 0."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def relays_next(self):
        """Sets next relay active. If none, returns 0."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def relays_read_monitoredterm(self):
        """Gets the number of terminal of monitored element that this relay is monitoring."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def relays_write_monitoredterm(self, argument):
        """Sets the number of terminal of monitored element that this relay is monitoring."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def relays_read_switchedterm(self):
        """Gets the number of terminal of the switched object that will be opened when the relay trips."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def relays_write_switchedterm(self, argument):
        """Sets the number of terminal of the switched object that will be opened when the relay trips."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def relays_read_idx(self):
        """Gets the active relay by index into the Relay list. 1..Count."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def relays_write_idx(self, argument):
        """Sets the active relay by index into the Relay list. 1..Count."""
        result = self.dss_obj.RelaysI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

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
