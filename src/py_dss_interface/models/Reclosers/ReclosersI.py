# -*- coding: iso-8859-15 -*-

import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Reclosers(Base):
    # ReclosersI (int)
    def reclosers_count(self):
        """Gets number of Reclosers in active circuit."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def reclosers_first(self):
        """Sets first recloser to be active Circuit Element. Returns 0 if none."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def reclosers_next(self):
        """Sets next recloser to be active Circuit Element. Returns 0 if none."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def reclosers_read_monitoredterm(self):
        """Gets the terminal number of Monitored Object for the Recloser."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def reclosers_write_monitoredterm(self, argument):
        """Sets the terminal number of Monitored Object for the Recloser."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def reclosers_read_switchedterm(self):
        """Gets the terminal of the controlled device being switched by the Recloser."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def reclosers_write_switchedterm(self, argument):
        """Sets the terminal of the controlled device being switched by the Recloser."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def reclosers_read_numfast(self):
        """Gets the number of fast shots."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def reclosers_write_numfast(self, argument):
        """Sets the number of fast shots."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def reclosers_read_shots(self):
        """Gets the number of shots to lockout (fast + delayed)."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def reclosers_write_shots(self, argument):
        """Sets the number of shots to lockout (fast + delayed)."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    def reclosers_open(self):
        """Open recloser's controlled element and lock out the recloser."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def reclosers_close(self):
        """Close the switched object controlled by the recloser. Resets recloser to first operation."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def reclosers_read_idx(self):
        """Gets the active recloser by index into the recloser list. 1..Count."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def reclosers_write_idx(self, argument):
        """Sets the active recloser by index into the recloser list. 1..Count."""
        result = self.dss_obj.ReclosersI(ctypes.c_int32(14), ctypes.c_int32(argument))
        return result

    # ReclosersF (Float)
    def reclosers_read_phasetrip(self):
        """Gets the phase trip curve multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def reclosers_write_phasetrip(self, argument):
        """Sets the phase trip curve multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def reclosers_read_phaseins(self):
        """Gets the phase instantaneous curve multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def reclosers_write_phaseins(self, argument):
        """Sets the phase instantaneous curve multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def reclosers_read_groundtrip(self):
        """Gets the ground (3I0) trip multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def reclosers_write_groundtrip(self, argument):
        """Sets the ground (3I0) trip multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def reclosers_read_groundinst(self):
        """Gets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def reclosers_write_groundinst(self, argument):
        """Sets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps."""
        result = float(self.dss_obj.ReclosersF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

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
