# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class FusesI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t FusesI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def fuses_count(self) -> int:
        """Returns the number of Fuses objects currently defined in the active circuit."""
        return self.dss_obj.FusesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def fuses_first(self) -> int:
        """Sets the first Fuse to be the active Fuse. Returns 0 if none."""
        return self.dss_obj.FusesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def fuses_next(self) -> int:
        """Sets the next Fuse to be the active Fuse. Returns 0 if none."""
        return self.dss_obj.FusesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def fuses_read_monitored_term(self) -> int:
        """Gets the terminal number to switch the fuse is connected."""
        return self.dss_obj.FusesI(ctypes.c_int32(3), ctypes.c_int32(0))

    def fuses_write_monitored_term(self, argument: int) -> int:
        """Sets the terminal number to switch the fuse is connected."""
        return self.dss_obj.FusesI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def fuses_read_switched_term(self) -> int:
        """Gets the terminal number of the terminal containing the switch controlled by the fuse."""
        return self.dss_obj.FusesI(ctypes.c_int32(5), ctypes.c_int32(0))

    def fuses_write_switched_term(self, argument: int) -> int:
        """Sets the terminal number of the terminal containing the switch controlled by the fuse."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.FusesI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def fuses_open(self) -> int:
        """Opening of fuse."""
        return self.dss_obj.FusesI(ctypes.c_int32(7), ctypes.c_int32(0))

    def fuses_close(self) -> int:
        """Closing of fuse."""
        return self.dss_obj.FusesI(ctypes.c_int32(8), ctypes.c_int32(0))

    def fuses_is_blown(self) -> int:
        """Returns the current state of the fuses. TRUE (1) if any on any phase is blown. Else FALSE (0)."""
        return self.dss_obj.FusesI(ctypes.c_int32(9), ctypes.c_int32(0))

    def fuses_read_idx(self) -> int:
        """Gets the active fuse by index into the list of fuses. 1 based: 1..count."""
        return self.dss_obj.FusesI(ctypes.c_int32(10), ctypes.c_int32(0))

    def fuses_write_idx(self, argument: int) -> int:
        """Sets the active fuse by index into the list of fuses. 1 based: 1..count."""
        argument = Base.check_int_param(argument, default=1)
        return self.dss_obj.FusesI(ctypes.c_int32(11), ctypes.c_int32(argument))

    def fuses_num_phases(self) -> int:
        """Gets the number of phases of the active fuse."""
        return self.dss_obj.FusesI(ctypes.c_int32(12), ctypes.c_int32(0))

    def fuses_reset(self):
        """Resets the state of the fuse object to the normal state."""
        result = self.dss_obj.FusesI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result
