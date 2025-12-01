# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.SwtControls.SwtControlsF import SwtControlsF
from py_dss_interface.models.SwtControls.SwtControlsI import SwtControlsI
from py_dss_interface.models.SwtControls.SwtControlsS import SwtControlsS
from py_dss_interface.models.SwtControls.SwtControlsV import SwtControlsV
from typing import List


class SwtControls(SwtControlsS, SwtControlsV, SwtControlsI, SwtControlsF):
    """
    This interface implements the SwtControls (ISwtControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: SwtControlsS, SwtControlsV, SwtControlsI,
    SwtControlsF.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def first(self) -> int:
        """Sets the first SwtControl active. Returns 0 if no more."""
        return SwtControlsI._first(self)

    def next(self) -> int:
        """Sets the next SwtControl active. Returns 0 if no more."""
        return SwtControlsI._next(self)

    def open(self) -> int:
        return SwtControlsI._open(self)

    def close(self) -> int:
        return SwtControlsI._close(self)

    def reset(self) -> int:
        return SwtControlsI._reset(self)

    @property
    def is_locked(self) -> int:
        """Gets the lock state: {1 locked | 0 not locked}.

        Sets the lock to prevent both manual and automatic switch operation. """
        return SwtControlsI._is_locked_read(self)

    @is_locked.setter
    def is_locked(self, value: int):
        SwtControlsI._is_locked_write(self, value)

    @property
    def switched_term(self) -> int:
        """Gets the terminal number where the switch is located on the SwitchedObj.

        Sets the terminal number where the switch is located on the SwitchedObj. """
        return SwtControlsI._switched_term_read(self)

    @switched_term.setter
    def switched_term(self, value: int):
        SwtControlsI._switched_term_write(self, value)

    @property
    def count(self) -> int:
        """Gets the total number of SwtControls in the active circuit."""
        return SwtControlsI._count(self)

    @property
    def name(self) -> str:
        """Gets the active swtcontrol name.

        Sets the active swtcontrol by name."""
        return SwtControlsS._name_read(self)

    @name.setter
    def name(self, value: str):
        SwtControlsS._name_write(self, value)

    @property
    def switched_obj(self) -> str:
        """Gets the name of the switched object by the active SwtControl.

        Sets the switched object by name."""
        return SwtControlsS._switched_obj_read(self)

    @switched_obj.setter
    def switched_obj(self, value: str):
        SwtControlsS._switched_obj_write(self, value)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings with all SwtControl names in the active circuit."""
        return SwtControlsV._names(self)

    @property
    def state(self) -> List[str]:
        return SwtControlsV._state_read(self)

    @state.setter
    def state(self, argument: List[str]):
        SwtControlsV._state_write(self, argument)

    @property
    def normal_state(self) -> List[str]:
        return SwtControlsV._normal_state_read(self)

    @normal_state.setter
    def normal_state(self, argument: List[str]):
        SwtControlsV._normal_state_write(self, argument)
