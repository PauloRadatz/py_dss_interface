# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Meters(Base):
    """
    This interface implements the Meters (IMeters) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: .
    """

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t MetersI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following. """

    def meters_first(self):
        """Sets the first Energy Meter active. Returns 0 if no monitors."""
        result = self.dss_obj.MetersI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def meters_next(self):
        """Sets the next energy Meter Active. Returns 0 if no more."""
        result = self.dss_obj.MetersI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def meters_reset(self):
        """Resets the active Meter object."""
        result = self.dss_obj.MetersI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def meters_resetall(self):
        """Resets all Meter object."""
        result = self.dss_obj.MetersI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def meters_sample(self):
        """Causes active meter to take a sample."""
        result = self.dss_obj.MetersI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def meters_save(self):
        """Causes active meter to save its current sample buffer to its meter stream.
        Then you can access the Bytestream or channel data. Most standard solution modes do this automatically."""
        result = self.dss_obj.MetersI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def meters_read_meteredterminal(self):
        """Returns the number of metered terminal by the active Energy Meter."""
        result = self.dss_obj.MetersI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def meters_write_meteredterminal(self, argument):
        """Sets the number of metered terminal by the active Energy Meter."""
        result = self.dss_obj.MetersI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def meters_difilesareopen(self):
        """Returns a global flag (1=true, 0=false) to indicate if Demand Interval (DI)
        files have been properly opened."""
        result = self.dss_obj.MetersI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def meters_sampleall(self):
        """Causes all Energy Meters to take a sample of the present state. Returns 0."""
        result = self.dss_obj.MetersI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def meters_saveall(self):
        """Save all Energy Meter buffers to their respective file streams. Returns 0."""
        result = self.dss_obj.MetersI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def meters_openalldifiles(self):
        """Opens Demand Interval (DI) files. Returns 0."""
        result = self.dss_obj.MetersI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def meters_closealldifiles(self):
        """Closes all Demand Interval (DI) files. Necessary at the end of a run."""
        result = self.dss_obj.MetersI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def meters_countendelements(self):
        """Returns the number of zone end elements in the active meter zone."""
        result = self.dss_obj.MetersI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def meters_count(self):
        """Returns the number of Energy Meters in the Active Circuit."""
        result = self.dss_obj.MetersI(ctypes.c_int32(14), ctypes.c_int32(0))
        return result

    def meters_countbranches(self):
        """Returns the number of branches in active Energy Meter zone (same as sequencelist size)."""
        result = self.dss_obj.MetersI(ctypes.c_int32(15), ctypes.c_int32(0))
        return result

    def meters_read_sequenceindex(self):
        """Returns the index into meter's SequenceList that contains branch pointers in lexical order.
        Earlier index guaranteed to be up line from later index. Sets PDElement active."""
        result = self.dss_obj.MetersI(ctypes.c_int32(16), ctypes.c_int32(0))
        return result

    def meters_write_sequenceindex(self, argument):
        """Sets the index into meter's SequenceList that contains branch pointers in lexical order.
        Earlier index guaranteed to be up line from later index. Sets PDElement active."""
        result = self.dss_obj.MetersI(ctypes.c_int32(17), ctypes.c_int32(argument))
        return result

    def meters_doreliabilitycalc(self):
        """Calculates SAIFI, etc. if the Argument is equal to 1 this parameter will assume restoration,
        otherwise it will not."""
        result = self.dss_obj.MetersI(ctypes.c_int32(18), ctypes.c_int32(0))
        return result

    def meters_seqlistsize(self):
        """Returns the size of Sequence List."""
        result = self.dss_obj.MetersI(ctypes.c_int32(19), ctypes.c_int32(0))
        return result

    def meters_totalcustomers(self):
        """Returns the total number of customers in this zone (down line from the Energy Meter)."""
        result = self.dss_obj.MetersI(ctypes.c_int32(20), ctypes.c_int32(0))
        return result

    def meters_numsections(self):
        """Returns the number of feeder sections in this meter's zone."""
        result = self.dss_obj.MetersI(ctypes.c_int32(21), ctypes.c_int32(0))
        return result

    def meters_setactivesection(self):
        """Sets the designated section (argument) if the index is valid."""
        result = self.dss_obj.MetersI(ctypes.c_int32(22), ctypes.c_int32(0))
        return result

    def meters_ocpdevicetype(self):
        """Returns the type of OCP device: {1=fuse | 2+ recloser | 3= relay}."""
        result = self.dss_obj.MetersI(ctypes.c_int32(23), ctypes.c_int32(0))
        return result

    def meters_numsectioncustomers(self):
        """Returns the number of customers in the active section."""
        result = self.dss_obj.MetersI(ctypes.c_int32(24), ctypes.c_int32(0))
        return result

    def meters_numsectionbranches(self):
        """Returns the number of branches (lines) in the active section."""
        result = self.dss_obj.MetersI(ctypes.c_int32(25), ctypes.c_int32(0))
        return result

    def meters_sectseqidx(self):
        """Returns the Sequence Index of the branch at the head of this section."""
        result = self.dss_obj.MetersI(ctypes.c_int32(26), ctypes.c_int32(0))
        return result

    def meters_secttotalcust(self):
        """Returns the total customers down line from this section."""
        result = self.dss_obj.MetersI(ctypes.c_int32(27), ctypes.c_int32(0))
        return result

    # MetersF (Float)
    def meters_saifi(self):
        """Returns SAIFI for this meter's zone. Execute reliability calc method first."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def meters_saifikw(self):
        """Returns the SAIFI based on kW rather than number of customers. Get after reliability calcs."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    def meters_saidi(self):
        """Returns the SAIDI for this meter zone. Execute DoreliabilityCalc first."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def meters_custinterrupts(self):
        """Returns the total customer interruptions for this meter zone based on reliability calcs."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def meters_avgrepairtime(self):
        """Returns the average Repair Time in this Section of the meter zone."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def meters_faultratexrepairhrs(self):
        """Returns the sum of Fault Rate Time Repair Hours in this section of the meter zone."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def meters_sumbranchfltrates(self):
        """Returns the sum of the branch fault rates in this section of the meter's zone."""
        result = float(self.dss_obj.MetersF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    # MetersS (String)
    def meters_read_name(self):
        """Returns the active Energy Meter's name."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def meters_write_name(self, argument):
        """Sets the active Energy Meter's name."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def meters_read_meteredelement(self):
        """Returns the name of the metered element (considering the active Energy Meter)."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def meters_write_meteredelement(self, argument):
        """Sets the name of the metered element (considering the active Energy Meter)."""
        result = ctypes.c_char_p(self.dss_obj.MetersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # MetersV (Variant)
    def meters_allnames(self):
        """Returns an array of all Energy Meter names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def meters_registernames(self):
        """Returns an array of strings containing the names of the registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def meters_registervalues(self):
        """Returns an array of values contained in the Meter registers for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def meters_totals(self):
        """Returns the totals for all registers of all Meters."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_peakcurrent(self):
        """Returns an array of doubles with the Peak Current Property."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_peakcurrent(self, argument):
        """Receives an array of doubles to set values of Peak Current Property."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MetersV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_calcurrent(self):
        """Returns the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_calcurrent(self, argument):
        """Sets the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MetersV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_allocfactors(self):
        """Returns an array of doubles: allocation factors for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_allocfactors(self, argument):
        """Receives an array of doubles to set the phase allocation factors for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.MetersV(ctypes.c_int(9), variant_pointer)
        return variant_pointer.contents.value

    def meters_allendelements(self):
        """Returns a variant array of names of all zone end elements."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(10), variant_pointer)
        return variant_pointer.contents.value

    def meters_allbranchesinzone(self):
        """Returns a wide string list of all branches in zone of the active Energy Meter object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.MetersV(ctypes.c_int(11), variant_pointer)
        return variant_pointer.contents.value
