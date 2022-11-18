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
        return PDElementsF._fault_rate_read(self)

    @fault_rate.setter
    def fault_rate(self, argument: float):
        PDElementsF._fault_rate_write(self, argument)

    @property
    def pct_permanent(self) -> float:
        return PDElementsF._pct_permanent_read(self)

    @pct_permanent.setter
    def pct_permanent(self, argument: float):
        PDElementsF._pct_permanent_write(self, argument)

    @property
    def branch_failure_rate(self) -> float:
        return PDElementsF._lambda(self)

    @property
    def accumulated_failure_rate(self) -> float:
        return PDElementsF._accumulated_failure_rate(self)

    @property
    def repair_time(self) -> float:
        return PDElementsF._repair_time(self)

    @property
    def total_miles(self) -> float:
        return PDElementsF._total_miles(self)

    @property
    def count(self) -> int:
        return PDElementsI._count(self)

    def first(self) -> int:
        return PDElementsI._first(self)

    def next(self) -> int:
        return PDElementsI._next(self)

    @property
    def is_shunt(self) -> int:
        return PDElementsI._is_shunt(self)

    @property
    def num_customers(self) -> int:
        return PDElementsI._num_customers(self)

    @property
    def total_customers(self) -> int:
        return PDElementsI._total_customers(self)

    @property
    def parent_pd_element(self) -> int:
        return PDElementsI._parent_pd_element(self)

    @property
    def from_terminal(self) -> int:
        return PDElementsI._from_terminal(self)

    @property
    def section_id(self) -> int:
        return PDElementsI._section_id(self)

    @property
    def name(self) -> str:
        return PDElementsS._name_read(self)

    @name.setter
    def name(self, argument: str):
        PDElementsS._name_write(self, argument)
