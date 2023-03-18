# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.DSSElement.DSSElementI import DSSElementI
from py_dss_interface.models.DSSElement.DSSElementS import DSSElementS
from py_dss_interface.models.DSSElement.DSSElementV import DSSElementV
from typing import List


class DSSElement(DSSElementI, DSSElementS, DSSElementV):
    """
    This interface implements the DSSElement (IDSSElement) interface of OpenDSS by declaring 3 procedures for
    accessing the different properties included in this interface: DSSElementI, DSSElementS, DSSElementV
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def num_properties(self) -> int:
        """Gets the number of properties for the active DSS object."""
        return DSSElementI._num_properties(self)

    @property
    def name(self) -> str:
        """Gets the full name of the active DSS object (general element or circuit element)."""
        return DSSElementS._name(self)

    @property
    def property_names(self) -> List[str]:
        """Gets a variant array of strings containing the names of all properties for the active DSS object."""
        return DSSElementV._property_names(self)
