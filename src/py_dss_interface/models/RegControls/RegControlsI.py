# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class RegControlsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t RegControlsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def regcontrols_first(self) -> int:
        """Sets the first RegControl active. Returns 0 if no more."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(0), ctypes.c_int32(0))

    def regcontrols_next(self) -> int:
        """Sets the next RegControl active. Returns 0 if no more"""
        return self.dss_obj.RegControlsI(ctypes.c_int32(1), ctypes.c_int32(0))

    def regcontrols_read_tap_winding(self) -> int:
        """Gets the tapped winding number."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(2), ctypes.c_int32(0))

    def regcontrols_write_tap_winding(self, argument) -> int:
        """Sets the tapped winding number."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def regcontrols_read_winding(self) -> int:
        """Gets the winding number for PT and CT connections."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(4), ctypes.c_int32(0))

    def regcontrols_write_winding(self, argument) -> int:
        """Sets the winding number for PT and CT connections."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))

    def regcontrols_read_is_reversible(self) -> int:
        """Gets the setting in the reverse direction, usually not applicable to substation transformers."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(6), ctypes.c_int32(0))

    def regcontrols_write_is_reversible(self, argument) -> int:
        """Sets the different settings for the reverse direction (see Manual for details),
        usually not applicable to substation transformers."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))

    def regcontrols_read_is_inverse_time(self) -> int:
        """Gets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(8), ctypes.c_int32(0))

    def regcontrols_write_is_inverse_time(self, argument) -> int:
        """Sets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(9), ctypes.c_int32(argument))

    def regcontrols_read_max_tap_change(self) -> int:
        """Gets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(10), ctypes.c_int32(0))

    def regcontrols_write_max_tap_change(self, argument) -> int:
        """Sets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(11), ctypes.c_int32(argument))

    def regcontrols_count(self) -> int:
        """Gets the number of RegControl objects in Active Circuit."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(12), ctypes.c_int32(0))

    def regcontrols_read_tap_number(self) -> int:
        """Gets the actual tap number of the active RegControl."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(13), ctypes.c_int32(0))

    def regcontrols_write_tap_number(self, argument) -> int:
        """Sets the actual tap number of the active RegControl."""
        return self.dss_obj.RegControlsI(ctypes.c_int32(14), ctypes.c_int32(argument))
