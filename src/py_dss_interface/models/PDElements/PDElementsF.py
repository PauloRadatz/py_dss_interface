# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class PDElements(Base):

    # PDElementsF (Float)
    def pdelements_read_faultrate(self):
        """Gets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def pdelements_write_faultrate(self, argument):
        """Sets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def pdelements_read_pctpermanent(self):
        """Gets the percent of faults that are permanent (require repair). Otherwise,
        fault is assumed to be transient/temporary."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def pdelements_write_pctpermanent(self, argument):
        """Sets the percent of faults that are permanent (require repair). Otherwise,
        fault is assumed to be transient/temporary."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def pdelements_lambda(self):
        """Gets the failure rate for this branch. Faults per year including length of line."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def pdelements_accumulatedl(self):
        """Gets the accumulated failure rate for this branch on down line."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def pdelements_repairtime(self):
        """Gets the average time to repair a permanent fault on this branch, hours."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def pdelements_totalmiles(self):
        """Gets the total miles of line from this element to the end of the zone. For recloser siting algorithm."""
        result = float(self.dss_obj.PDElementsF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    # PDElementsS (String)
    def pdelements_read_name(self):
        """Gets the name of the active PDElement, returns null string if active element id not PDElement."""
        result = ctypes.c_char_p(self.dss_obj.PDElementsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def pdelements_write_name(self, argument):
        """Sets the name of the active PDElement, returns null string if active element id not PDElement."""
        result = ctypes.c_char_p(self.dss_obj.PDElementsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')
