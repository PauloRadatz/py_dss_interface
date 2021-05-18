# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MonitorsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t MonitorsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def monitors_first(self):
        """Sets the first monitor active. Returns 0 if no monitors."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def monitors_next(self):
        """Set the next monitor active. Returns 0 if no more."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def monitors_reset(self):
        """Resets the active Monitor object."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def monitors_reset_all(self):
        """Resets all Monitor object."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def monitors_sample(self):
        """Causes active monitor to take a sample."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def monitors_save(self):
        """Causes active monitor to save its current sample buffer to its monitor stream.
        Then you can access the Bytestream or channel data. Most standard solution modes do this automatically."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def monitors_show(self):
        """Converts monitor file into text and displays with text editor."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def monitors_read_mode(self):
        """Returns the monitor mode (bitmask integer - see DSS Help)."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def monitors_write_mode(self, argument):
        """Sets the monitor mode (bitmask integer - see DSS Help)."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def monitors_sample_count(self):
        """Returns number of examples in Monitor at present."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def monitors_sample_all(self):
        """Causes all Monitors to take a sample of the present state. Returns 0."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def monitors_save_all(self):
        """Save all Monitor buffers to their respective file streams. Returns 0."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def monitors_count(self):
        """Returns the number of monitors."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def monitors_process(self):
        """Post-process monitor examples taken so far, e.g., Pst for mode = 4."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def monitors_process_all(self):
        """Makes that all monitors post-process the data taken so far."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(14), ctypes.c_int32(0))
        return result

    def monitors_file_version(self):
        """Returns the Monitor File version (integer)."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(15), ctypes.c_int32(0))
        return result

    def monitors_record_size(self):
        """Returns the size of each record in ByteStream."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(16), ctypes.c_int32(0))
        return result

    def monitors_num_channels(self):
        """Returns the number of Channels on the active Monitor."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(17), ctypes.c_int32(0))
        return result

    def monitors_read_terminal(self):
        """Returns the terminal number of element being monitored."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(18), ctypes.c_int32(0))
        return result

    def monitors_write_terminal(self, argument):
        """Sets sets the terminal number of element being monitored."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(19), ctypes.c_int32(argument))
        return result
