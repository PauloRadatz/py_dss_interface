# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models.Base import Base


class SwtControlsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t SwtControlsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def swtcontrols_first(self) -> int:
        """Sets the first SwtControl active. Returns 0 if no more."""
        return self.dss_obj.SwtControlsI(ctypes.c_int32(0), ctypes.c_int32(0))

    def swtcontrols_next(self) -> int:
        """Sets the next SwtControl active. Returns 0 if no more."""
        return self.dss_obj.SwtControlsI(ctypes.c_int32(1), ctypes.c_int32(0))

    def swtcontrols_read_action(self) -> int:
        """Gets the open (1) or close (2) action of the switch. No effect if switch is locked.
        However, reset removes any lock and then closes the switch (shelf state). 0 = none action."""
        return self.dss_obj.SwtControlsI(ctypes.c_int32(2), ctypes.c_int32(0))

    def swtcontrols_write_action(self, argument) -> int:
        """Sets open (1) or close (2) the switch. No effect if switch is locked. However,
        reset removes any lock and then closes the switch (shelf state). 0 = none action (see manual for details). """
        argument = Base.check_int_param(argument)
        return self.dss_obj.SwtControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def swtcontrols_read_is_locked(self) -> int:
        """Gets the lock state: {1 locked | 0 not locked}."""
        return self.dss_obj.SwtControlsI(ctypes.c_int32(4), ctypes.c_int32(0))

    def swtcontrols_write_is_locked(self, argument) -> int:
        """Sets the lock to prevent both manual and automatic switch operation. """
        argument = Base.check_int_param(argument)
        return self.dss_obj.SwtControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))

    def swtcontrols_read_switched_term(self) -> int:
        """Gets the terminal number where the switch is located on the SwitchedObj."""
        return self.dss_obj.SwtControlsI(ctypes.c_int32(6), ctypes.c_int32(0))

    def swtcontrols_write_switched_term(self, argument) -> int:
        """Sets the terminal number where the switch is located on the SwitchedObj. """
        argument = Base.check_int_param(argument)
        return self.dss_obj.SwtControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))

    def swtcontrols_count(self) -> int:
        """Gets the total number of SwtControls in the active circuit."""
        return self.dss_obj.SwtControlsI(ctypes.c_int32(8), ctypes.c_int32(0))
