# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
import numpy as np
from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class MonitorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.
    The structure of the interface is as follows:
        void MonitorsV(int32_t Parameter, VARIANT *Argument);
    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def monitors_all_names(self):
        """Returns an array of all Monitor names (array of strings)."""
        return Bridge.var_array_function(self.dss_obj.MonitorsV, ctypes.c_int(0), ctypes.c_int(0), None)

    def monitors_byte_stream(self):
        """Returns a byte array containing monitor stream values. Make sure a "save" is done first (standard solution
        modes do this automatically). """
        return Bridge.var_array_function(self.dss_obj.MonitorsV, ctypes.c_int(1), ctypes.c_int(0), None)

    def monitors_header(self):
        """Returns the header string; Variant array of strings containing Channel Names."""
        return Bridge.var_array_function(self.dss_obj.MonitorsV, ctypes.c_int(2), ctypes.c_int(0), None)

    def monitors_dbl_hour(self):
        """Returns returns a variant array of doubles containing time value in hours for the time-sampled monitor
        values; empty if frequency-sampled values for harmonics solution (see dblFreq)."""
        return Bridge.var_array_function(self.dss_obj.MonitorsV, ctypes.c_int(3), ctypes.c_int(0), None)

    def monitors_dbl_freq(self):
        """Returns a variant array of doubles containing time values for harmonics mode solutions; empty for time
        mode solutions (use dblHour). """
        return Bridge.var_array_function(self.dss_obj.MonitorsV, ctypes.c_int(4), ctypes.c_int(0), None)

    def monitors_channel(self, argument) -> str:
        """Returns a variant array of doubles for the specified channel (usage: MyArray = DSSmonitor. Channel(i)) A
        save or SaveAll should be executed first. Done automatically by most standard solution modes. """
        r = np.array(self.monitors_byte_stream())
        r = np.reshape(r, (len(self.monitors_dbl_hour()), len(self.monitors_header()) + 2))
        # return r[:, [0, 1, argument+1]]
        return list(r[:, argument + 1])

