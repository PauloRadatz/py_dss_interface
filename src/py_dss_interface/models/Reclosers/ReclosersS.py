# -*- coding: iso-8859-15 -*-

import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Reclosers(Base):
    """
    This interface implements the Reclosers (IReclosers) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    # ReclosersS (String)
    def reclosers_read_name(self):
        """Gets the name of the active Recloser Object."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_name(self, argument):
        """Sets the name of the active Recloser Object."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def reclosers_read_monitoredobj(self):
        """Gets the full name of object this Recloser is monitoring."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_monitoredobj(self, argument):
        """Sets the full name of object this Recloser is monitoring."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def reclosers_read_switchedobj(self):
        """Gets the full name of the circuit element that is being switched by this Recloser."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_switchedobj(self, argument):
        """Sets the full name of the circuit element that is being switched by this Recloser."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

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
