# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class ISources(Base):
    # IsourcesI (int)
    def isources_count(self):
        """Returns the number of Isource objects currently defined in the active circuit."""
        result = self.dss_obj.IsourceI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def isources_first(self):
        """Sets the first ISource to be active; returns 0 if none."""
        result = self.dss_obj.IsourceI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def isources_next(self):
        """Sets the next ISource to be active; returns 0 if none."""
        result = self.dss_obj.IsourceI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    # IsourcesF (Float)
    def isources_read_amps(self):
        """Gets the magnitude of the Isource in Amps."""
        result = float(self.dss_obj.IsourceF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def isources_write_amps(self, argument):
        """Sets the magnitude of the Isource in Amps."""
        result = float(self.dss_obj.IsourceF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def isources_read_angledeg(self):
        """Gets the phase angle of the Isource in degrees."""
        result = float(self.dss_obj.IsourceF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def isources_write_angledeg(self, argument):
        """Sets the phase angle of the Isource in degrees."""
        result = float(self.dss_obj.IsourceF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def isources_read_frequency(self):
        """Gets the frequency of the Isource in Hz."""
        result = float(self.dss_obj.IsourceF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def isources_write_frequency(self, argument):
        """Sets the frequency of the Isource in Hz."""
        result = float(self.dss_obj.IsourcesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    # IsourcesS (String)
    def isources_read_name(self):
        """Gets the name of the active Isource object."""
        result = ctypes.c_char_p(self.dss_obj.IsourceS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def isources_write_name(self, argument):
        """Sets the name of the active Isource object."""
        result = ctypes.c_char_p(self.dss_obj.IsourceS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # IsourcesV (Variant)
    def isources_allnames(self):
        """Gets the variant array of string containing names of all ISources in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.IsourceV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
