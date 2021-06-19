# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class PDElementsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double PDElementsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.

    """

    def pdelements_read_fault_rate(self) -> float:
        """Gets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def pdelements_write_fault_rate(self, argument) -> float:
        """Sets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def pdelements_read_pct_permanent(self) -> float:
        """Gets the percent of faults that are permanent (require repair). Otherwise,
        fault is assumed to be transient/temporary."""
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def pdelements_write_pct_permanent(self, argument) -> float:
        """Sets the percent of faults that are permanent (require repair). Otherwise, fault is assumed to be
        transient/temporary. """
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def pdelements_lambda(self) -> float:
        """Gets the failure rate for this branch. Faults per year including length of line."""
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(4), ctypes.c_double(0)))

    def pdelements_accumulated_l(self) -> float:
        """Gets the accumulated failure rate for this branch on down line."""
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(5), ctypes.c_double(0)))

    def pdelements_repair_time(self) -> float:
        """Gets the average time to repair a permanent fault on this branch, hours."""
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(6), ctypes.c_double(0)))

    def pdelements_total_miles(self) -> float:
        """Gets the total miles of line from this element to the end of the zone. For recloser siting algorithm."""
        return float(self.dss_obj.PDElementsF(ctypes.c_int32(7), ctypes.c_double(0)))
