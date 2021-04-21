# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Fuses(Base):

    # FusesI (int)
    def fuses_count(self):
        """Returns the number of Fuses objects currently defined in the active circuit."""
        result = self.dss_obj.FusesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def fuses_first(self):
        """Sets the first Fuse to be the active Fuse. Returns 0 if none."""
        result = self.dss_obj.FusesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def fuses_next(self):
        """Sets the next Fuse to be the active Fuse. Returns 0 if none."""
        result = self.dss_obj.FusesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def fuses_read_monitoredterm(self):
        """Gets the terminal number to switch the fuse is connected."""
        result = self.dss_obj.FusesI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def fuses_write_monitoredterm(self, argument):
        """Sets the terminal number to switch the fuse is connected."""
        result = self.dss_obj.FusesI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def fuses_read_switchedterm(self):
        """Gets the terminal number of the terminal containing the switch controlled by the fuse."""
        result = self.dss_obj.FusesI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def fuses_write_switchedterm(self, argument):
        """Sets the terminal number of the terminal containing the switch controlled by the fuse."""
        result = self.dss_obj.FusesI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def fuses_open(self):
        """Opening of fuse."""
        result = self.dss_obj.FusesI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def fuses_close(self):
        """Closing of fuse."""
        result = self.dss_obj.FusesI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def fuses_isblown(self):
        """Returns the current state of the fuses. TRUE (1) if any on any phase is blown. Else FALSE (0)."""
        result = self.dss_obj.FusesI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def fuses_read_idx(self):
        """Gets the active fuse by index into the list of fuses. 1 based: 1..count."""
        result = self.dss_obj.FusesI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def fuses_write_idx(self, argument):
        """Sets the active fuse by index into the list of fuses. 1 based: 1..count."""
        result = self.dss_obj.FusesI(ctypes.c_int32(11), ctypes.c_int32(argument))
        return result

    def fuses_numphases(self):
        """Gets the number of phases of the active fuse."""
        result = self.dss_obj.FusesI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    # FusesF (Float)
    def fuses_read_ratedcurrent(self):
        """Gets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0,
        Multiply current values of TCC curve by this to get actual amps."""
        result = float(self.dss_obj.FusesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def fuses_write_ratedcurrent(self, argument):
        """Sets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0,
        Multiply current values of TCC curve by this to get actual amps."""
        result = float(self.dss_obj.FusesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def fuses_read_delay(self):
        """Gets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0."""
        result = float(self.dss_obj.FusesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def fuses_write_delay(self, argument):
        """Sets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0."""
        result = float(self.dss_obj.FusesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    # FusesS (String)
    def fuses_read_name(self):
        """Gets the name of the active fuse."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_name(self, argument):
        """Sets the name of the active fuse."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def fuses_read_monitoredobj(self):
        """Gets the name of the Monitored Object by the active fuse."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_monitoredobj(self, argument):
        """Sets the name of the Monitored Object by the active fuse."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def fuses_read_switchedobj(self):
        """Gets the full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_switchedobj(self, argument):
        """Sets the full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def fuses_read_tcccurve(self):
        """Gets the name of the TCCcurve object that determines fuse blowing."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_tcccurve(self, argument):
        """Sets the name of the TCCcurve object that determines fuse blowing."""
        result = ctypes.c_char_p(self.dss_obj.FusesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    # FusesV (Variant)
    def fuses_allnames(self):
        """Gets the variant array of string containing names of all fuses in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.FusesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
