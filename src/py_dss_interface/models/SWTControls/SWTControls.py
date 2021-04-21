# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class SWTControls(Base):

    # SwtControlsI (int)
    def swtcontrols_first(self):
        """Sets the first SwtControl active. Returns 0 if no more."""
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def swtcontrols_next(self):
        """Sets the next SwtControl active. Returns 0 if no more."""
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def swtcontrols_read_action(self):
        """Gets the open (1) or close (2) action of the switch. No effect if switch is locked.
        However, reset removes any lock and then closes the switch (shelf state). 0 = none action."""
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def swtcontrols_write_action(self, argument):
        """Sets open (1) or close (2) the switch. No effect if switch is locked. However,
        reset removes any lock and then closes the switch (shelf state). 0 = none action (see manual for details). """
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def swtcontrols_read_islocked(self):
        """Gets the lock state: {1 locked | 0 not locked}."""
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def swtcontrols_write_islocked(self, argument):
        """Sets the lock to prevent both manual and automatic switch operation. """
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def swtcontrols_read_switchedterm(self):
        """Gets the terminal number where the switch is located on the SwitchedObj."""
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def swtcontrols_write_switchedterm(self, argument):
        """Sets the terminal number where the switch is located on the SwitchedObj. """
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def swtcontrols_count(self):
        """Gets the total number of SwtControls in the active circuit."""
        result = self.dss_obj.SwtControlsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    # SwtControlsF (Float)
    def swtcontrols_read_delay(self):
        """Gets the time delay [s] between arming and opening or closing the switch.
        Control may reset before actually operating the switch."""
        result = float(self.dss_obj.SwtControlsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def swtcontrols_write_delay(self, argument):
        """Sets sets the time delay [s] between arming and opening or closing the switch.
        Control may reset before actually operating the switch."""
        result = float(self.dss_obj.SwtControlsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    # SwtControlsS (string)
    def swtcontrols_read_name(self):
        """Gets the active swtcontrol name."""
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def swtcontrols_write_name(self, argument):
        """Sets the active swtcontrol by name."""
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def swtcontrols_read_switchedobj(self):
        """Gets the name of the switched object by the active SwtControl """
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def swtcontrols_write_switchedobj(self, argument):
        """Sets the switched object by name."""
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SwtControlsV (Variant)
    def swtcontrols_allnames(self):
        """Gets a variant array of strings with all SwtControl names in the active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SwtControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
