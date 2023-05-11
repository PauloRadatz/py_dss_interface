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
        """Gets the number of circuits currently defined."""
        return DSSInterfaceI._num_circuits(self)

    def clear_all(self):
        """Clears all circuit definitions."""
        return DSSInterfaceI._clear_all(self)

    def show_panel(self):
        """Shows non-MDI child form of the Main DSS Edit form."""
        return DSSInterfaceI._show_panel(self)

    def start(self) -> int:
        """Validates the user and starts the DSS. Returns TRUE (1) if successful."""
        return DSSInterfaceI._start(self)

    @property
    def num_classes(self) -> int:
        """Gets the number of DSS intrinsic classes."""
        return DSSInterfaceI._num_classes(self)

    @property
    def num_user_classes(self) -> int:
        """Gets the number of user-defined classes."""
        return DSSInterfaceI._num_user_classes(self)

    def reset(self):
        """Resets DSS initialization for restarts, etc. from applets. Nothing implemented in the OpenDSS Original
                Source Code """
        return DSSInterfaceI._reset(self)

    @property
    def allow_forms(self) -> int:
        """Gets if the DSS allows forms (1) or not (0), default (1).

        Sets if the DSS allows forms (1) or not (0), default (1). PAY ATTENTION: If arg=0 Then NoFormsAllowed :=
                TRUE (Only set to False) else NoFormsAllowed := FALSE; """
        return DSSInterfaceI._allow_forms_read(self)

    @allow_forms.setter
    def allow_forms(self, value: int):
        DSSInterfaceI._allow_forms_write(self, value)

    def new_circuit(self, argument: str) -> str:
        """Makes a new circuit, the name of the circuit must be specified in the Argument."""
        return DSSInterfaceS._new_circuit(self, argument)

    @property
    def version(self) -> str:
        """Gets the version string for the DSS."""
        return DSSInterfaceS._version(self)

    @property
    def datapath(self) -> str:
        """Gets the Data File Path. Default for reports, etc. from DSS.

        Sets the Data File Path. Default for reports, etc. from DSS."""
        return DSSInterfaceS._datapath_read(self)

    @datapath.setter
    def datapath(self, argument: str):
        DSSInterfaceS._datapath_write(self, argument)

    @property
    def default_editor(self) -> str:
        """Gets the path name for the default text editor."""
        return DSSInterfaceS._default_editor(self)

    @property
    def classes(self) -> List[str]:
        """Gets the list of DSS intrinsic classes (names of the classes)."""
        return DSSInterfaceV._classes(self)

    @property
    def user_classes(self) -> List[str]:
        """Gets list of user-defined classes (names of the classes)."""
        return DSSInterfaceV._user_classes(self)
