# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Monitors.MonitorsI import MonitorsI
from py_dss_interface.models.Monitors.MonitorsS import MonitorsS
from py_dss_interface.models.Monitors.MonitorsV import MonitorsV
from typing import List


class Monitors(MonitorsI, MonitorsV, MonitorsS):
    """
    This interface implements the Monitors (IMonitors) interface of OpenDSS by declaring 3 procedures for accessing
    the different properties included in this interface: MonitorsI._, MonitorsV, MonitorsS.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def first(self) -> int:
        """Sets the first monitor active. Returns 0 if no monitors."""
        return MonitorsI._first(self)

    def next(self) -> int:
        """Set the next monitor active. Returns 0 if no more."""
        return MonitorsI._next(self)

    def reset(self) -> int:
        """Resets the active Monitor object."""
        return MonitorsI._reset(self)

    def reset_all(self) -> int:
        """Resets all Monitor object."""
        return MonitorsI._reset_all(self)

    def sample(self) -> int:
        """Causes active monitor to take a sample."""
        return MonitorsI._sample(self)

    def save(self) -> int:
        """Causes active monitor to save its current sample buffer to its monitor stream.
                Then you can access the Bytestream or channel data. Most standard solution modes do this automatically."""
        return MonitorsI._save(self)

    def show(self) -> int:
        """Converts monitor file into text and displays with text editor."""
        return MonitorsI._show(self)

    @property
    def mode(self) -> int:
        """Returns the monitor mode (bitmask integer - see DSS Help).

        Sets the monitor mode (bitmask integer - see DSS Help).

        Bitmask integer designating the values the monitor is to capture:

            0 = Voltages and currents at designated terminal

            1 = Powers at designated terminal

            2 = Tap Position (Transformer Device only)
            3 = State Variables (PCElements only)

            4 = Flicker level and severity index (Pst) for voltages. No adders apply.
                Flicker level at simulation time step, Pst at 10-minute time step.

            5 = Solution variables (Iterations, etc).
            Normally, these would be actual phasor quantities from solution.

            6 = Capacitor Switching (Capacitors only)

            7 = Storage state vars (Storage device only)

            8 = All winding currents (Transformer device only)

            9 = Losses, watts and var (of monitored device)

            10 = All Winding voltages (Transformer device only)
            Normally, these would be actual phasor quantities from solution.

            11 = All terminal node voltages and line currents of monitored device
            Combine mode with adders below to achieve other results for terminal quantities:

            +16 = Sequence quantities

            +32 = Magnitude only

            +64 = Positive sequence only or avg of all phases

        Mix adder to obtain desired results. For example:

            Mode=112 will save positive sequence voltage and current magnitudes only

            Mode=48 will save all sequence voltages and currents, but magnitude only.
        """
        return MonitorsI._mode(self)

    @mode.setter
    def mode(self, argument):
        MonitorsI._mode_write(self, argument)

    @property
    def sample_count(self) -> int:
        """Returns number of examples in Monitor at present."""
        return MonitorsI._sample_count(self)

    def sample_all(self) -> int:
        """Causes all Monitors to take a sample of the present state. Returns 0."""
        return MonitorsI._sample_all(self)

    def save_all(self) -> int:
        """Save all Monitor buffers to their respective file streams. Returns 0."""
        return MonitorsI._save_all(self)

    @property
    def count(self) -> int:
        """Returns the number of monitors."""
        return MonitorsI._count(self)

    def process(self) -> int:
        """Post-process monitor examples taken so far, e.g., Pst for mode = 4."""
        return MonitorsI._process(self)

    def process_all(self) -> int:
        """Makes that all monitors post-process the data taken so far."""
        return MonitorsI._process_all(self)

    @property
    def file_version(self) -> int:
        """Returns the Monitor File version (integer)."""
        return MonitorsI._file_version(self)

    @property
    def record_size(self) -> int:
        """Returns the size of each record in ByteStream."""
        return MonitorsI._record_size(self)

    @property
    def num_channels(self) -> int:
        """Returns the number of Channels on the active Monitor."""
        return MonitorsI._num_channels(self)

    @property
    def terminal(self) -> int:
        """Returns the terminal number of element being monitored.

        Sets sets the terminal number of element being monitored."""
        return MonitorsI._terminal(self)

    @terminal.setter
    def terminal(self, argument: int):
        MonitorsI._terminal_write(self, argument)

    @property
    def file_name(self) -> str:
        """Returns the name of the CSV file associated with active monitor."""
        return MonitorsS._file_name(self)

    @property
    def name(self) -> str:
        """Returns the active Monitor object by name.

        Sets the active Monitor object by name."""
        return MonitorsS._name_read(self)

    @name.setter
    def name(self, argument: str):
        MonitorsS._name_write(self, argument)

    @property
    def element(self) -> str:
        """Returns the full name of element being monitored by the active Monitor.

        Sets the full name of element being monitored by the active Monitor."""
        return MonitorsS._element_read(self)

    @element.setter
    def element(self, argument: str):
        MonitorsS._element_write(self, argument)

    @property
    def names(self) -> List[str]:
        """Returns an array of all Monitor names (array of strings)."""
        return MonitorsV._names(self)

    @property
    def byte_stream(self) -> List[int]:
        """Returns a byte array containing monitor stream values. Make sure a "save" is done first (standard solution
                modes do this automatically). """
        return MonitorsV._byte_stream(self)

    @property
    def header(self) -> List[str]:
        """Returns the header string; Variant array of strings containing Channel Names."""
        return MonitorsV._header(self)

    @property
    def dbl_hour(self) -> List[float]:
        """Returns returns a variant array of doubles containing time value in hours for the time-sampled monitor
                values; empty if frequency-sampled values for harmonics solution (see dblFreq)."""
        return MonitorsV._dbl_hour(self)

    @property
    def dbl_freq(self) -> List[float]:
        """Returns a variant array of doubles containing time values for harmonics mode solutions; empty for time
                mode solutions (use dblHour). """
        return MonitorsV._dbl_freq(self)

    def channel(self, argument) -> List[float]:
        """Returns a variant array of doubles for the specified channel (usage: MyArray = DSSmonitor. Channel(i)) A
                save or SaveAll should be executed first. Done automatically by most standard solution modes. """
        return MonitorsV._channel(self, argument)
