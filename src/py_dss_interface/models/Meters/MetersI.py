# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MetersI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t MetersI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following. """

    def meters_first(self) -> int:
        """Sets the first Energy Meter active. Returns 0 if no monitors."""
        return self.dss_obj.MetersI(ctypes.c_int32(0), ctypes.c_int32(0))

    def meters_next(self) -> int:
        """Sets the next energy Meter Active. Returns 0 if no more."""
        return self.dss_obj.MetersI(ctypes.c_int32(1), ctypes.c_int32(0))

    def meters_reset(self) -> int:
        """Resets the active Meter object."""
        return self.dss_obj.MetersI(ctypes.c_int32(2), ctypes.c_int32(0))

    def meters_resetall(self) -> int:
        """Resets all Meter object."""
        return self.dss_obj.MetersI(ctypes.c_int32(3), ctypes.c_int32(0))

    def meters_sample(self) -> int:
        """Causes active meter to take a sample."""
        return self.dss_obj.MetersI(ctypes.c_int32(4), ctypes.c_int32(0))

    def meters_save(self) -> int:
        """Causes active meter to save its current sample buffer to its meter stream. Then you can access the
        Bytestream or channel data. Most standard solution modes do this automatically. """
        return self.dss_obj.MetersI(ctypes.c_int32(5), ctypes.c_int32(0))

    def meters_read_meteredterminal(self) -> int:
        """Returns the number of metered terminal by the active Energy Meter."""
        return self.dss_obj.MetersI(ctypes.c_int32(6), ctypes.c_int32(0))

    def meters_write_meteredterminal(self, argument) -> int:
        """Sets the number of metered terminal by the active Energy Meter."""
        return self.dss_obj.MetersI(ctypes.c_int32(7), ctypes.c_int32(argument))

    def meters_difilesareopen(self) -> int:
        """Returns a global flag (1=true, 0=false) to indicate if Demand Interval (DI) files have been properly
        opened. """
        return self.dss_obj.MetersI(ctypes.c_int32(8), ctypes.c_int32(0))

    def meters_sampleall(self) -> int:
        """Causes all Energy Meters to take a sample of the present state. Returns 0."""
        return self.dss_obj.MetersI(ctypes.c_int32(9), ctypes.c_int32(0))

    def meters_saveall(self) -> int:
        """Save all Energy Meter buffers to their respective file streams. Returns 0."""
        return self.dss_obj.MetersI(ctypes.c_int32(10), ctypes.c_int32(0))

    def meters_openalldifiles(self) -> int:
        """Opens Demand Interval (DI) files. Returns 0."""
        return self.dss_obj.MetersI(ctypes.c_int32(11), ctypes.c_int32(0))

    def meters_closealldifiles(self) -> int:
        """Closes all Demand Interval (DI) files. Necessary at the end of a run."""
        return self.dss_obj.MetersI(ctypes.c_int32(12), ctypes.c_int32(0))

    def meters_countendelements(self) -> int:
        """Returns the number of zone end elements in the active meter zone."""
        return self.dss_obj.MetersI(ctypes.c_int32(13), ctypes.c_int32(0))

    def meters_count(self) -> int:
        """Returns the number of Energy Meters in the Active Circuit."""
        return self.dss_obj.MetersI(ctypes.c_int32(14), ctypes.c_int32(0))

    def meters_countbranches(self) -> int:
        """Returns the number of branches in active Energy Meter zone (same as sequencelist size)."""
        return self.dss_obj.MetersI(ctypes.c_int32(15), ctypes.c_int32(0))

    def meters_read_sequenceindex(self) -> int:
        """Returns the index into meter's SequenceList that contains branch pointers in lexical order. Earlier index
        guaranteed to be up line from later index. Sets PDElement active. """
        return self.dss_obj.MetersI(ctypes.c_int32(16), ctypes.c_int32(0))

    def meters_write_sequenceindex(self, argument) -> int:
        """Sets the index into meter's SequenceList that contains branch pointers in lexical order. Earlier index
        guaranteed to be up line from later index. Sets PDElement active. """
        return self.dss_obj.MetersI(ctypes.c_int32(17), ctypes.c_int32(argument))

    def meters_doreliabilitycalc(self) -> int:
        """Calculates SAIFI, etc. if the Argument is equal to 1 this parameter will assume restoration, otherwise it
        will not. """
        return self.dss_obj.MetersI(ctypes.c_int32(18), ctypes.c_int32(0))

    def meters_seqlistsize(self) -> int:
        """Returns the size of Sequence List."""
        return self.dss_obj.MetersI(ctypes.c_int32(19), ctypes.c_int32(0))

    def meters_totalcustomers(self) -> int:
        """Returns the total number of customers in this zone (down line from the Energy Meter)."""
        return self.dss_obj.MetersI(ctypes.c_int32(20), ctypes.c_int32(0))

    def meters_numsections(self) -> int:
        """Returns the number of feeder sections in this meter's zone."""
        return self.dss_obj.MetersI(ctypes.c_int32(21), ctypes.c_int32(0))

    def meters_setactivesection(self) -> int:
        """Sets the designated section (argument) if the index is valid."""
        return self.dss_obj.MetersI(ctypes.c_int32(22), ctypes.c_int32(0))

    def meters_ocpdevicetype(self) -> int:
        """Returns the type of OCP device: {1=fuse | 2+ recloser | 3= relay}."""
        return self.dss_obj.MetersI(ctypes.c_int32(23), ctypes.c_int32(0))

    def meters_numsectioncustomers(self) -> int:
        """Returns the number of customers in the active section."""
        return self.dss_obj.MetersI(ctypes.c_int32(24), ctypes.c_int32(0))

    def meters_numsectionbranches(self) -> int:
        """Returns the number of branches (lines) in the active section."""
        return self.dss_obj.MetersI(ctypes.c_int32(25), ctypes.c_int32(0))

    def meters_sectseqidx(self) -> int:
        """Returns the Sequence Index of the branch at the head of this section."""
        return self.dss_obj.MetersI(ctypes.c_int32(26), ctypes.c_int32(0))

    def meters_secttotalcust(self) -> int:
        """Returns the total customers down line from this section."""
        return self.dss_obj.MetersI(ctypes.c_int32(27), ctypes.c_int32(0))
