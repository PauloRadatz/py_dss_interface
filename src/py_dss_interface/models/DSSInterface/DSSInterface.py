# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.DSSInterface.DSSInterfaceI import DSSInterfaceI
from py_dss_interface.models.DSSInterface.DSSInterfaceS import DSSInterfaceS
from py_dss_interface.models.DSSInterface.DSSInterfaceV import DSSInterfaceV


class DSSInterface(DSSInterfaceS, DSSInterfaceI, DSSInterfaceV):
    """
    This interface implements the DSS interface (IDSS - DDSS.pas) of OpenDSS by declaring 3 procedures for accessing the
    different properties included in this interface: DSSInterfaceS, DSSInterfaceI, DSSInterfaceV
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def allow_forms(self) -> int:
        return DSSInterfaceI._allow_forms(self)

    @allow_forms.setter
    def allow_forms(self, value: int):
        DSSInterfaceI._allow_forms_write(self, value)
