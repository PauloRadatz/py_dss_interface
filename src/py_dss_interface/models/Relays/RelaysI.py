# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class RelaysI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t RelaysI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def relays_count(self) -> int:
        """Gets number of Relays in active circuit."""
        return self.dss_obj.RelaysI(ctypes.c_int32(0), ctypes.c_int32(0))

    def relays_first(self) -> int:
        """Sets first relay active. If none, returns 0."""
        return self.dss_obj.RelaysI(ctypes.c_int32(1), ctypes.c_int32(0))

    def relays_next(self) -> int:
        """Sets next relay active. If none, returns 0."""
        return self.dss_obj.RelaysI(ctypes.c_int32(2), ctypes.c_int32(0))

    def relays_read_monitored_term(self) -> int:
        """Gets the number of terminal of monitored element that this relay is monitoring."""
        return self.dss_obj.RelaysI(ctypes.c_int32(3), ctypes.c_int32(0))

    def relays_write_monitored_term(self, argument) -> int:
        """Sets the number of terminal of monitored element that this relay is monitoring."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.RelaysI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def relays_read_switched_term(self) -> int:
        """Gets the number of terminal of the switched object that will be opened when the relay trips."""
        return self.dss_obj.RelaysI(ctypes.c_int32(5), ctypes.c_int32(0))

    def relays_write_switched_term(self, argument) -> int:
        """Sets the number of terminal of the switched object that will be opened when the relay trips."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.RelaysI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def relays_read_idx(self) -> int:
        """Gets the active relay by index into the Relay list. 1..Count."""
        return self.dss_obj.RelaysI(ctypes.c_int32(7), ctypes.c_int32(0))

    def relays_write_idx(self, argument) -> int:
        """Sets the active relay by index into the Relay list. 1..Count."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.RelaysI(ctypes.c_int32(8), ctypes.c_int32(argument))
