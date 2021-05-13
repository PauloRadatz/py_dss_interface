# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Monitors(Base):

    # MonitorsI (int)
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

    def monitors_resetall(self):
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

    def monitors_samplecount(self):
        """Returns number of samples in Monitor at present."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def monitors_sampleall(self):
        """Causes all Monitors to take a sample of the present state. Returns 0."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def monitors_saveall(self):
        """Save all Monitor buffers to their respective file streams. Returns 0."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def monitors_count(self):
        """Returns the number of monitors."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def monitors_process(self):
        """Post-process monitor samples taken so far, e.g., Pst for mode = 4."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def monitors_processall(self):
        """Makes that all monitors post-process the data taken so far."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(14), ctypes.c_int32(0))
        return result

    def monitors_fileversion(self):
        """Returns the Monitor File version (integer)."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(15), ctypes.c_int32(0))
        return result

    def monitors_recordsize(self):
        """Returns the size of each record in ByteStream."""
        result = self.dss_obj.MonitorsI(ctypes.c_int32(16), ctypes.c_int32(0))
        return result

    def monitors_numchannels(self):
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

    # MonitorsS (String)
    def monitors_filename(self):
        """Returns the name of the CSV file associated with active monitor."""
        result = ctypes.c_char_p(self.dss_obj.MonitorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def monitors_read_name(self):
        """Returns the active Monitor object by name."""
        result = ctypes.c_char_p(self.dss_obj.MonitorsS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def monitors_write_name(self, argument):
        """Sets the active Monitor object by name."""
        result = ctypes.c_char_p(self.dss_obj.MonitorsS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def monitors_read_element(self):
        """Returns the full name of element being monitored by the active Monitor."""
        result = ctypes.c_char_p(self.dss_obj.MonitorsS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def monitors_write_element(self, argument):
        """Sets the full name of element being monitored by the active Monitor."""
        result = ctypes.c_char_p(self.dss_obj.MonitorsS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    # MonitorsV (Variant)
    def monitors_allnames(self):
        """Returns an array of all Monitor names (array of strings)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def monitors_bytestream(self):
        """Returns a byte array containing monitor stream values.
        Make sure a "save" is done first (standard solution modes do this automatically)."""
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
        """Returns a variant array of doubles containing time values for harmonics mode solutions;
        empty for time mode solutions (use dblHour)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MonitorsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def monitors_channel(self, argument):
        """Returns a variant array of doubles for the specified channel (usage: MyArray = DSSmonitor.
        Channel(i)) A save or SaveAll should be executed first. Done automatically by most standard solution modes."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MonitorsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value
