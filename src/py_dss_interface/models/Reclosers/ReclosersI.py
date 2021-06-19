# -*- coding: iso-8859-15 -*-

import ctypes

from py_dss_interface.models.Base import Base


class ReclosersI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t ReclosersI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def reclosers_count(self) -> int:
        """Gets number of Reclosers in active circuit."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(0), ctypes.c_int32(0))

    def reclosers_first(self) -> int:
        """Sets first recloser to be active Circuit Element. Returns 0 if none."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(1), ctypes.c_int32(0))

    def reclosers_next(self) -> int:
        """Sets next recloser to be active Circuit Element. Returns 0 if none."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(2), ctypes.c_int32(0))

    def reclosers_read_monitored_term(self) -> int:
        """Gets the terminal number of Monitored Object for the Recloser."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(3), ctypes.c_int32(0))

    def reclosers_write_monitored_term(self, argument) -> int:
        """Sets the terminal number of Monitored Object for the Recloser."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def reclosers_read_switched_term(self) -> int:
        """Gets the terminal of the controlled device being switched by the Recloser."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(5), ctypes.c_int32(0))

    def reclosers_write_switched_term(self, argument) -> int:
        """Sets the terminal of the controlled device being switched by the Recloser."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def reclosers_read_num_fast(self) -> int:
        """Gets the number of fast shots."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(7), ctypes.c_int32(0))

    def reclosers_write_num_fast(self, argument) -> int:
        """Sets the number of fast shots."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(8), ctypes.c_int32(argument))

    def reclosers_read_shots(self) -> int:
        """Gets the number of shots to lockout (fast + delayed)."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(9), ctypes.c_int32(0))

    def reclosers_write_shots(self, argument) -> int:
        """Sets the number of shots to lockout (fast + delayed)."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(10), ctypes.c_int32(argument))

    def reclosers_open(self) -> int:
        """Open recloser's controlled element and lock out the recloser."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(11), ctypes.c_int32(0))

    def reclosers_close(self) -> int:
        """Close the switched object controlled by the recloser. Resets recloser to first operation."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(12), ctypes.c_int32(0))

    def reclosers_read_idx(self) -> int:
        """Gets the active recloser by index into the recloser list. 1..Count."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(13), ctypes.c_int32(0))

    def reclosers_write_idx(self, argument) -> int:
        """Sets the active recloser by index into the recloser list. 1..Count."""
        return self.dss_obj.ReclosersI(ctypes.c_int32(14), ctypes.c_int32(argument))
