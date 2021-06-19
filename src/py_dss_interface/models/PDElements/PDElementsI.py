# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class PDElementsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t PDElementsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def pdelements_count(self) -> int:
        """Gets number of PDElements in active circuit."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(0), ctypes.c_int32(0))

    def pdelements_first(self) -> int:
        """Sets the first enabled PD element to be the active element. Returns 0 if none found."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(1), ctypes.c_int32(0))

    def pdelements_next(self) -> int:
        """Sets the next enabled PD element to be the active element. Returns 0 if none found."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(2), ctypes.c_int32(0))

    def pdelements_is_shunt(self) -> int:
        """Sets returns 1 if the PD element should be treated as a shunt element rather than a series element.
        Applies to capacitor and reactor elements in particular. """
        return self.dss_obj.PDElementsI(ctypes.c_int32(3), ctypes.c_int32(0))

    def pdelements_num_customers(self) -> int:
        """Gets the number of customers in this branch."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(4), ctypes.c_int32(0))

    def pdelements_total_customers(self) -> int:
        """Gets the total number of customers from this branch to the end of the zone."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(5), ctypes.c_int32(0))

    def pdelements_parent_pd_element(self) -> int:
        """Gets the parent PD element to be the active circuit element. Returns 0 if no more elements upline."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(6), ctypes.c_int32(0))

    def pdelements_from_terminal(self) -> int:
        """Gets the number of the terminal of active PD element that is on the "from" side.
        This is set after the meter zone is determined."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(7), ctypes.c_int32(0))

    def pdelements_section_id(self) -> int:
        """Gets the integer ID of the feeder section that this PDElement branch is part of."""
        return self.dss_obj.PDElementsI(ctypes.c_int32(8), ctypes.c_int32(0))
