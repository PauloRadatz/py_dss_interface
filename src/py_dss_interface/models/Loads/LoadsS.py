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
