# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.DSSInterface.DSSInterfaceI import DSSInterfaceI
from py_dss_interface.models.DSSInterface.DSSInterfaceS import DSSInterfaceS
from py_dss_interface.models.DSSInterface.DSSInterfaceV import DSSInterfaceV
from typing import List


class DSSInterface(DSSInterfaceS, DSSInterfaceI, DSSInterfaceV):
    """
    This interface implements the DSS interface (IDSS - DDSS.pas) of OpenDSS by declaring 3 procedures for accessing the
    different properties included in this interface: DSSInterfaceS, DSSInterfaceI, DSSInterfaceV
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def num_circuits(self) -> int:
        return DSSInterfaceI._num_circuits(self)

    def clear_all(self):
        return DSSInterfaceI._clear_all(self)

    def show_panel(self):
        return DSSInterfaceI._show_panel(self)

    def start(self) -> int:
        return DSSInterfaceI._start(self)

    @property
    def num_classes(self) -> int:
        return DSSInterfaceI._num_classes(self)

    @property
    def num_user_classes(self) -> int:
        return DSSInterfaceI._num_user_classes(self)

    def reset(self):
        return DSSInterfaceI._reset(self)

    @property
    def allow_forms(self) -> int:
        return DSSInterfaceI._allow_forms_read(self)

    @allow_forms.setter
    def allow_forms(self, value: int):
        DSSInterfaceI._allow_forms_write(self, value)

    def new_circuit(self, argument: str) -> str:
        return DSSInterfaceS._new_circuit(self, argument)

    @property
    def version(self) -> str:
        return DSSInterfaceS._version(self)

    @property
    def datapath(self) -> str:
        return DSSInterfaceS._datapath_read(self)

    @datapath.setter
    def datapath(self, argument: str):
        DSSInterfaceS._datapath_write(self, argument)

    @property
    def default_editor(self) -> str:
        return DSSInterfaceS._default_editor(self)

    @property
    def classes(self) -> List[str]:
        return DSSInterfaceV._classes(self)

    @property
    def user_classes(self) -> List[str]:
        return DSSInterfaceV._user_classes(self)
