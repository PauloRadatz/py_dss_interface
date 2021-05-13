# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class PVSystemsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t PVSystemsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def pvsystems_count(self) -> int:
        """Returns the number of PVSystem objects currently defined in the active circuit."""
        return int(self.dss_obj.PVsystemsI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def pvsystems_first(self) -> int:
        """Sets the first PVSystem to be active; returns 0 if none."""
        return int(self.dss_obj.PVsystemsI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def pvsystems_next(self) -> int:
        """Sets the next PVSystem to be active; returns 0 if none."""
        return int(self.dss_obj.PVsystemsI(ctypes.c_int32(2), ctypes.c_int32(0)))

    def pvsystems_read_idx(self) -> int:
        """Gets the active PVSystem by index; 1..Count."""
        return int(self.dss_obj.PVsystemsI(ctypes.c_int32(3), ctypes.c_int32(0)))

    def pvsystems_write_idx(self, argument) -> int:
        """Sets the active PVSystem by index; 1..Count.."""
        return int(self.dss_obj.PVsystemsI(ctypes.c_int32(4), ctypes.c_int32(argument)))
