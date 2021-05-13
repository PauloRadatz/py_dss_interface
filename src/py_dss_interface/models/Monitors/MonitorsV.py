# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class MonitorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void MonitorsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def monitors_allnames(self):
        """Returns an array of all Monitor names (array of strings)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def monitors_bytestream(self):
        """Returns a byte array containing monitor stream values. Make sure a "save" is done first (standard solution
        modes do this automatically). """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def monitors_header(self):
        """Returns the header string; Variant array of strings containing Channel Names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def monitors_dblhour(self):
        """Returns returns a variant array of doubles containing time value in hours for the time-sampled monitor
        values; empty if frequency-sampled values for harmonics solution (see dblFreq)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def monitors_dblfreq(self):
        """Returns a variant array of doubles containing time values for harmonics mode solutions; empty for time
        mode solutions (use dblHour). """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def monitors_channel(self, argument):
        """Returns a variant array of doubles for the specified channel (usage: MyArray = DSSmonitor. Channel(i)) A
        save or SaveAll should be executed first. Done automatically by most standard solution modes. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MonitorsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value
