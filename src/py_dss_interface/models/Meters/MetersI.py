# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base
from py_dss_interface.utils.Error import Error


class MetersI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t MetersI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following. """

    def _first(self) -> int:
        """Sets the first Energy Meter active. Returns 0 if no monitors."""
        return self._dss_obj.MetersI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _next(self) -> int:
        """Sets the next energy Meter Active. Returns 0 if no more."""
        return self._dss_obj.MetersI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _reset(self) -> int:
        """Resets the active Meter object."""
        return self._dss_obj.MetersI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _reset_all(self) -> int:
        """Resets all Meter object."""
        return self._dss_obj.MetersI(ctypes.c_int32(3), ctypes.c_int32(0))

    def _sample(self) -> int:
        """Causes active meter to take a sample."""
        return self._dss_obj.MetersI(ctypes.c_int32(4), ctypes.c_int32(0))

    def _save(self) -> int:
        """Causes active meter to save its current sample buffer to its meter stream. Then you can access the
        Bytestream or channel data. Most standard solution modes do this automatically. """
        return self._dss_obj.MetersI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _metered_terminal_read(self) -> int:
        """Returns the number of metered terminal by the active Energy Meter."""
        return self._dss_obj.MetersI(ctypes.c_int32(6), ctypes.c_int32(0))

    def _metered_terminal_write(self, argument) -> int:
        """Sets the number of metered terminal by the active Energy Meter."""
        return self._dss_obj.MetersI(ctypes.c_int32(7), ctypes.c_int32(argument))

    # TODO include in test
    def _di_files_are_open(self) -> int:
        """Returns a global flag (1=true, 0=false) to indicate if Demand Interval (DI) files have been properly
        opened. """
        return self._dss_obj.MetersI(ctypes.c_int32(8), ctypes.c_int32(0))

    def _sample_all(self) -> int:
        """Causes all Energy Meters to take a sample of the present state. Returns 0."""
        return self._dss_obj.MetersI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _save_all(self) -> int:
        """Save all Energy Meter buffers to their respective file streams. Returns 0."""
        return self._dss_obj.MetersI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _open_all_di_files(self) -> int:
        """Opens Demand Interval (DI) files. Returns 0."""
        return self._dss_obj.MetersI(ctypes.c_int32(11), ctypes.c_int32(0))

    def _close_all_di_files(self) -> int:
        """Closes all Demand Interval (DI) files. Necessary at the end of a run."""
        return self._dss_obj.MetersI(ctypes.c_int32(12), ctypes.c_int32(0))

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/15
    def _count_end_elements(self) -> int:
        """Returns the number of zone end elements in the active meter zone."""
        return self._dss_obj.MetersI(ctypes.c_int32(13), ctypes.c_int32(0))

    def _count(self) -> int:
        """Returns the number of Energy Meters in the Active Circuit."""
        return self._dss_obj.MetersI(ctypes.c_int32(14), ctypes.c_int32(0))

    def _count_branches(self) -> int:
        """Returns the number of branches in active Energy Meter zone (same as sequencelist size)."""
        return self._dss_obj.MetersI(ctypes.c_int32(15), ctypes.c_int32(0))

    def _sequence_index_read(self) -> int:
        """Returns the index into meter's SequenceList that contains branch pointers in lexical order. Earlier index
        guaranteed to be up line from later index. Sets PDElement active. """
        return self._dss_obj.MetersI(ctypes.c_int32(16), ctypes.c_int32(0))

    def _sequence_index_write(self, argument) -> int:
        """Sets the index into meter's SequenceList that contains branch pointers in lexical order. Earlier index
        guaranteed to be up line from later index. Sets PDElement active. """
        return self._dss_obj.MetersI(ctypes.c_int32(17), ctypes.c_int32(argument))

    def _do_reliability_calc(self) -> int:
        """Calculates SAIFI, etc. if the Argument is equal to 1 this parameter will assume restoration, otherwise it
        will not. """
        return self._dss_obj.MetersI(ctypes.c_int32(18), ctypes.c_int32(0))

    def _seq_list_size(self) -> int:
        """Returns the size of Sequence List."""
        return self._dss_obj.MetersI(ctypes.c_int32(19), ctypes.c_int32(0))

    def _total_customers(self) -> int:
        """Returns the total number of customers in this zone (down line from the Energy Meter)."""
        return self._dss_obj.MetersI(ctypes.c_int32(20), ctypes.c_int32(0))

    def _num_sections_read(self) -> int:
        """Returns the number of feeder sections in this meter's zone."""
        return self._dss_obj.MetersI(ctypes.c_int32(21), ctypes.c_int32(0))

    def _set_active_section_write(self, argument) -> int:
        """Sets the designated section (argument) if the index is valid."""
        # return self.dss_obj.MetersI(ctypes.c_int32(22), ctypes.c_int32(argument))
        Error.method_not_working("dss.meters.set_active_section")

    def _ocp_device_type(self) -> int:
        """Returns the type of OCP device: {1=fuse | 2+ recloser | 3= relay}."""
        return self._dss_obj.MetersI(ctypes.c_int32(23), ctypes.c_int32(0))

    def _num_section_customers(self) -> int:
        """Returns the number of customers in the active section."""
        return self._dss_obj.MetersI(ctypes.c_int32(24), ctypes.c_int32(0))

    def _num_section_branches(self) -> int:
        """Returns the number of branches (lines) in the active section."""
        return self._dss_obj.MetersI(ctypes.c_int32(25), ctypes.c_int32(0))

    def _sect_seq_idx(self) -> int:
        """Returns the Sequence Index of the branch at the head of this section."""
        return self._dss_obj.MetersI(ctypes.c_int32(26), ctypes.c_int32(0))

    def _sect_total_cust(self) -> int:
        """Returns the total customers down line from this section."""
        return self._dss_obj.MetersI(ctypes.c_int32(27), ctypes.c_int32(0))
