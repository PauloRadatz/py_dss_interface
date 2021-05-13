# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Loads(Base):
    """
    This interface implements the Loads (ILoads) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: .
    """

    # Loads F (Float)
    def loads_read_kw(self):
        """Allows to read the kW property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def loads_write_kw(self, argument):
        """Allows to write the kW property of the active load.
        The parameter argument must contain the new value in kW for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def loads_read_kv(self):
        """Allows to read the kV property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def loads_write_kv(self, argument):
        """Allows to write the kV property of the active load.
        The parameter argument must contain the new value in kV for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def loads_read_kvar(self):
        """Allows to read the kvar property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def loads_write_kvar(self, argument):
        """Allows to write the kvar property of the active load.
        The parameter argument must contain the new value in kvar for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def loads_read_pf(self):
        """Allows to read the pf property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def loads_write_pf(self, argument):
        """Allows to write the pf property of the active load.
        The parameter argument must contain the new value in pf for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def loads_read_pctmean(self):
        """Allows to read the PctMean property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def loads_write_pctmean(self, argument):
        """Allows to write the PctMean property of the active load.
        The parameter argument must contain the new value in PctMean for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def loads_read_pctstddev(self):
        """Allows to read the PctStdDev property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def loads_write_pctstddev(self, argument):
        """Allows to write the PctStdDev property of the active load.
        The parameter argument must contain the new value in PctStdDev for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def loads_read_allocationfactor(self):
        """Allows to read the AllocationFactor property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def loads_write_allocationfactor(self, argument):
        """Allows to write the AllocationFactor property of the active load.
        The parameter argument must contain the new value in AllocationFactor for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def loads_read_cfactor(self):
        """Allows to read the CFactor property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def loads_write_cfactor(self, argument):
        """Allows to write the CFactor property of the active load.
        The parameter argument must contain the new value in CFactor for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def loads_read_cvrwatts(self):
        """Allows to read the CVRWatts property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def loads_write_cvrwatts(self, argument):
        """Allows to write the CVRWatts property of the active load.
        The parameter argument must contain the new value in CVRWatts for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def loads_read_cvrvars(self):
        """Allows to read the CVRvars property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def loads_write_cvrvars(self, argument):
        """Allows to write the CVRvars property of the active load.
        The parameter argument must contain the new value in CVRWatts for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def loads_read_kva(self):
        """Allows to read the kva property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def loads_write_kva(self, argument):
        """Allows to write the kva property of the active load.
        The parameter argument must contain the new value in kva for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def loads_read_kwh(self):
        """Allows to read the kWh property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def loads_write_kwh(self, argument):
        """Allows to write the kWh property of the active load.
        The parameter argument must contain the new value in kWh for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(23), ctypes.c_double(argument)))
        return result

    def loads_read_kwhdays(self):
        """Allows to read the kWhdays property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    def loads_write_kwhdays(self, argument):
        """Allows to write the kWhdays property of the active load.
        The parameter argument must contain the new value in kWhdays for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(25), ctypes.c_double(argument)))
        return result

    def loads_read_rneut(self):
        """Allows to read the RNeut (neutral resistance for wye connected loads) property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(26), ctypes.c_double(0)))
        return result

    def loads_write_rneut(self, argument):
        """Allows to write the RNeut (neutral resistance for wye connected loads) property of the active load.
        The parameter argument must contain the new value in RNeut for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(27), ctypes.c_double(argument)))
        return result

    def loads_read_vmaxpu(self):
        """Allows to read the VMaxpu property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(28), ctypes.c_double(0)))
        return result

    def loads_write_vmaxpu(self, argument):
        """Allows to write the VMaxpu property of the active load.
        The parameter argument must contain the new value in VMaxpu for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(29), ctypes.c_double(argument)))
        return result

    def loads_read_vminemerg(self):
        """Allows to read the VMinemerg property of the active load. The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(30), ctypes.c_double(0)))
        return result

    def loads_write_vminemerg(self, argument):
        """Allows to write the VMinemerg property of the active load.
        The parameter argument must contain the new value in VMinemerg for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(31), ctypes.c_double(argument)))
        return result

    def loads_read_vminnorm(self):
        """Allows to read the VMinnorm property of the active load. The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(32), ctypes.c_double(0)))
        return result

    def loads_write_vminnorm(self, argument):
        """Allows to write the VMinnorm property of the active load.
        The parameter argument must contain the new value in VMinnorm for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(33), ctypes.c_double(argument)))
        return result

    def loads_read_vminpu(self):
        """Allows to read the VMinpu property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(34), ctypes.c_double(0)))
        return result

    def loads_write_vminpu(self, argument):
        """Allows to write the VMinpu property of the active load.
        The parameter argument must contain the new value in VMinpu for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(35), ctypes.c_double(argument)))
        return result

    def loads_read_xfkva(self):
        """Allows to read the xfKVA (Rated service transformer KVA for load allocation, using Allocationfactor.
        Affects kW, kvar and pf.) property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(36), ctypes.c_double(0)))
        return result

    def loads_write_xfkva(self, argument):
        """Allows to write the xfKVA (Rated service transformer KVA for load allocation, using Allocationfactor.
        Affects kW, kvar and pf.) property of the active load.
        The parameter argument must contain the new value in xfKVA for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(37), ctypes.c_double(argument)))
        return result

    def loads_read_xneut(self):
        """Allows to read the Xneut property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(38), ctypes.c_double(0)))
        return result

    def loads_write_xneut(self, argument):
        """Allows to write the Xneut property of the active load.
        The parameter argument must contain the new value in Xneut for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(39), ctypes.c_double(argument)))
        return result

    def loads_read_pctseriesrl(self):
        """allows to read the PctSeriesRL (Percent of Load that is modeled as series R-L for harmonic studies)
        property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(40), ctypes.c_double(0)))
        return result

    def loads_write_pctseriesrl(self, argument):
        """allows to write the PctSeriesRL (Percent of Load that is modeled as series R-L for harmonic studies)
        property of the active load.
        The parameter argument must contain the new value in PctSeriesRL for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(41), ctypes.c_double(argument)))
        return result

    def loads_read_relweight(self):
        """Allows to read the RelWeight (relative weighting factor) property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(42), ctypes.c_double(0)))
        return result

    def loads_write_relweight(self, argument):
        """Allows to write the RelWeight (relative weighting factor) property of the active load.
        The parameter argument must contain the new value in RelWeight for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dss_obj.DSSLoadsF(ctypes.c_int32(43), ctypes.c_double(argument)))
        return result

    # LoadsS (String)
    def loads_read_name(self):
        """Allows to read the Name property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_name(self, argument):
        """allows to set the active load by specifying the Name load.
        The parameter argument must contain the Name of the load to activate.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_cvrcurve(self):
        """Allows to read the CVRCurve property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_cvrcurve(self, argument):
        """Allows to set the CVRCurve property for the active load.
        The parameter argument must contain the Name of the new CVRCurve to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_daily(self):
        """Allows to read the daily property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_daily(self, argument):
        """Allows to set the daily property for the active load.
        The parameter argument must contain the Name of the new daily to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_duty(self):
        """Allows to read the duty property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_duty(self, argument):
        """Allows to set the dduty property for the active load.
        The parameter argument must contain the Name of the new duty to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_spectrum(self):
        """Allows to read the Spectrum property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_spectrum(self, argument):
        """Allows to set the Spectrum property for the active load.
        The parameter argument must contain the Name of the new Spectrum to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_yearly(self):
        """Allows to read the Yearly property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_yearly(self, argument):
        """Allows to set the Yearly property for the active load.
        The parameter argument must contain the Name of the new Yearly to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_growth(self):
        """Allows to read the Growth property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(12), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_growth(self, argument):
        """Allows to set the Growth property for the active load.
        The parameter argument must contain the Name of the new Growth to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(13), argument.encode('ascii')))
        return result.value.decode('ascii')

    # LoadsV (Variant)
    def loads_allnames(self):
        """Allows to read the names of all the loads present in the active circuit.
        The result is delivered as variant, however, the content of this variant is an array of strings."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSLoadsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def loads_read_zipv(self):
        """Allows to read the array of 7 elements (doubles) for ZIP property of the active Load object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSLoadsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def loads_write_zipv(self, argument):
        """Allows to write the array of 7 elements (doubles) for ZIP property of the active Load object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.DSSLoadsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
