# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
import numpy as np
from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List

class MonitorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.
    The structure of the interface is as follows:
        void MonitorsV(int32_t Parameter, VARIANT *Argument);
    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _names(self) -> List[str]:
        """Returns an array of all Monitor names (array of strings)."""
        return Bridge.variant_pointer_read(self.dss_obj.MonitorsV, 0)

    def _byte_stream(self) -> List[int]:
        """Returns a byte array containing monitor stream values. Make sure a "save" is done first (standard solution
        modes do this automatically). """
        return Bridge.variant_pointer_read(self.dss_obj.MonitorsV, 1)

    def _header(self) -> List[str]:
        """Returns the header string; Variant array of strings containing Channel Names."""
        return Bridge.variant_pointer_read(self.dss_obj.MonitorsV, 2)

    def _dbl_hour(self) -> List[float]:
        """Returns returns a variant array of doubles containing time value in hours for the time-sampled monitor
        values; empty if frequency-sampled values for harmonics solution (see dblFreq)."""
        return Bridge.variant_pointer_read(self.dss_obj.MonitorsV, 3)

    def _dbl_freq(self) -> List[float]:
        """Returns a variant array of doubles containing time values for harmonics mode solutions; empty for time
        mode solutions (use dblHour). """
        return Bridge.variant_pointer_read(self.dss_obj.MonitorsV, 4)

    def _channel(self, arg: int) -> List[float]:
        """Returns a variant array of doubles for the specified channel (usage: MyArray = DSSmonitor. Channel(i)) A
        save or SaveAll should be executed first. Done automatically by most standard solution modes. """
        return Bridge.variant_pointer_read(self.dss_obj.MonitorsV, 5, arg)

