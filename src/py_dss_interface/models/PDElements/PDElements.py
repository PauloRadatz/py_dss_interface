# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.PDElements.PDElementsF import PDElementsF
from py_dss_interface.models.PDElements.PDElementsI import PDElementsI
from py_dss_interface.models.PDElements.PDElementsS import PDElementsS


class PDElements(PDElementsF, PDElementsI, PDElementsS):
    """
    This interface implements the PDElements (IPDElements) interface of OpenDSS by declaring 3 procedures for
    accessing the different properties included in this interface: PDElementsF, PDElementsI, PDElementsS.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def fault_rate(self) -> float:
        """Gets the number of failures per year. For LINE elements: Number of failures per unit length per year.

        Sets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        return PDElementsF._fault_rate_read(self)

    @fault_rate.setter
    def fault_rate(self, argument: float):
        PDElementsF._fault_rate_write(self, argument)

    @property
    def pct_permanent(self) -> float:
        """Gets the percent of faults that are permanent (require repair). Otherwise,
                fault is assumed to be transient/temporary.

        Sets the percent of faults that are permanent (require repair). Otherwise, fault is assumed to be
        transient/temporary. """
        return PDElementsF._pct_permanent_read(self)

    @pct_permanent.setter
    def pct_permanent(self, argument: float):
        PDElementsF._pct_permanent_write(self, argument)

    @property
    def branch_failure_rate(self) -> float:
        """Gets the failure rate for this branch. Faults per year including length of line."""
        return PDElementsF._lambda(self)

    @property
    def accumulated_failure_rate(self) -> float:
        """Gets the accumulated failure rate for this branch on down line."""
        return PDElementsF._accumulated_failure_rate(self)

    @property
    def repair_time(self) -> float:
        """Gets the average time to repair a permanent fault on this branch, hours."""
        return PDElementsF._repair_time(self)

    @property
    def total_miles(self) -> float:
        """Gets the total miles of line from this element to the end of the zone. For recloser siting algorithm."""
        return PDElementsF._total_miles(self)

    @property
    def count(self) -> int:
        """Gets number of PDElements in active circuit."""
        return PDElementsI._count(self)

    def first(self) -> int:
        """Sets the first enabled PD element to be the active element. Returns 0 if none found."""
        return PDElementsI._first(self)

    def next(self) -> int:
        """Sets the next enabled PD element to be the active element. Returns 0 if none found."""
        return PDElementsI._next(self)

    @property
    def is_shunt(self) -> int:
        """Sets returns 1 if the PD element should be treated as a shunt element rather than a series element.
                Applies to capacitor and reactor elements in particular. """
        return PDElementsI._is_shunt(self)

    @property
    def num_customers(self) -> int:
        """Gets the number of customers in this branch."""
        return PDElementsI._num_customers(self)

    @property
    def total_customers(self) -> int:
        """Gets the total number of customers from this branch to the end of the zone."""
        return PDElementsI._total_customers(self)

    @property
    def parent_pd_element(self) -> int:
        """Gets the parent PD element to be the active circuit element. Returns 0 if no more elements upline."""
        return PDElementsI._parent_pd_element(self)

    @property
    def from_terminal(self) -> int:
        """Gets the number of the terminal of active PD element that is on the "from" side.
                This is set after the meter zone is determined."""
        return PDElementsI._from_terminal(self)

    @property
    def section_id(self) -> int:
        """Gets the integer ID of the feeder section that this PDElement branch is part of."""
        return PDElementsI._section_id(self)

    @property
    def name(self) -> str:
        """Gets the name of the active PDElement, returns null string if active element id not PDElement.

        Sets the name of the active PDElement, returns null string if active element id not PDElement."""
        return PDElementsS._name_read(self)

    @name.setter
    def name(self, argument: str):
        PDElementsS._name_write(self, argument)
