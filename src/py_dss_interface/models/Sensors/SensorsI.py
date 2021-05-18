# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class SensorsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t SensorsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def sensors_count(self) -> int:
        """Gets number of sensors in active circuit."""
        return self.dss_obj.SensorsI(ctypes.c_int32(0), ctypes.c_int32(0))

    def sensors_first(self) -> int:
        """Sets the first sensor active. Returns 0 if none."""
        return self.dss_obj.SensorsI(ctypes.c_int32(1), ctypes.c_int32(0))

    def sensors_next(self) -> int:
        """Sets the next sensor active. Returns 0 if none."""
        return self.dss_obj.SensorsI(ctypes.c_int32(2), ctypes.c_int32(0))

    def sensors_read_is_delta(self) -> int:
        """Returns 1 if the sensor is connected in delta; otherwise, returns 0."""
        return self.dss_obj.SensorsI(ctypes.c_int32(3), ctypes.c_int32(0))

    def sensors_write_is_delta(self, argument) -> int:
        """Allows to set 1 if the sensor is connected in delta; otherwise, set 0 (argument)."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.SensorsI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def sensors_read_reverse_delta(self) -> int:
        """Returns 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        return self.dss_obj.SensorsI(ctypes.c_int32(5), ctypes.c_int32(0))

    def sensors_write_reverse_delta(self, argument) -> int:
        """Allows to set 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.SensorsI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def sensors_read_metered_terminal(self) -> int:
        """Gets the number of the measured terminal in the measured element."""
        return self.dss_obj.SensorsI(ctypes.c_int32(7), ctypes.c_int32(0))

    def sensors_write_metered_terminal(self, argument) -> int:
        """Sets the number of the measured terminal in the measured element."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.SensorsI(ctypes.c_int32(8), ctypes.c_int32(argument))

    def sensors_reset(self) -> int:
        """Clears the active sensor."""
        return self.dss_obj.SensorsI(ctypes.c_int32(9), ctypes.c_int32(0))

    def sensors_reset_all(self) -> int:
        """Clears all sensors in the active circuit."""
        return self.dss_obj.SensorsI(ctypes.c_int32(10), ctypes.c_int32(0))
