# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Capacitors.CapacitorsF import CapacitorsF
from py_dss_interface.models.Capacitors.CapacitorsI import CapacitorsI
from py_dss_interface.models.Capacitors.CapacitorsS import CapacitorsS
from py_dss_interface.models.Capacitors.CapacitorsV import CapacitorsV


class Capacitors(CapacitorsF, CapacitorsI, CapacitorsS, CapacitorsV):
    """
    This interface implements the Capacitors (ICapacitors) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: CapacitorsF, CapacitorsI, CapacitorsS, CapacitorsV.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def num_steps(self) -> int:
        """Gets the number of steps (defaults 1) for distributing and switching the total bank kvar.

        Sets the number of steps (defaults 1) for distributing and switching the total bank kvar.
        """
        return CapacitorsI._num_steps(self)

    @num_steps.setter
    def num_steps(self, argument: int):
        CapacitorsI._num_steps_write(self, argument)

    @property
    def is_delta(self) -> int:
        """Gets 1 if delta connection, otherwise will return 0 for distributing and switching the total kvar.

        Sets (Argument) 1 if delta connection, otherwise will return 0 for distributing and switching the total
        kvar."""
        return CapacitorsI.is_delta(self)

    @is_delta.setter
    def is_delta(self, argument: int):
        CapacitorsI._is_delta_write(self, argument)

    @property
    def count(self) -> int:
        """Gets the number of capacitor objects in active circuit."""
        return CapacitorsI._count(self)

    @property
    def available_steps(self) -> int:
        """Gets the number of steps available in cap bank to be switched ON."""
        return CapacitorsI._available_steps(self)

    def open_all_steps(self) -> int:
        """Opens all steps, all phases of the capacitor."""
        return CapacitorsI._open_all_steps(self)

    def close_all_steps(self) -> int:
        """Closes all steps, all phases of the capacitor."""
        return CapacitorsI._close_all_steps(self)

    @property
    def kv(self) -> float:
        """Gets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.

        Sets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase. There is not a explicit
        return type in the oficial documentation, because of this we choose not put a explicit return too."""
        return CapacitorsF._kv(self)

    @kv.setter
    def kv(self, argument: float):
        CapacitorsF._kv_write(self, argument)

    @property
    def kvar(self) -> float:
        """Gets the total bank kvar, distributed equally among phases and steps.

        Sets the total bank kvar, distributed equally among phases and steps. There is not a explicit return type
        in the oficial documentation, because of this we choose not put a explicit return too. """
        return CapacitorsF._kvar(self)

    @kvar.setter
    def kvar(self, argument: float):
        CapacitorsF._kvar_write(self, argument)

    @property
    def name(self) -> str:
        """Gets the name of the active Capacitor element.

        Sets the name of the Capacitor element to set it active. There is not a explicit return type in the
        oficial documentation, because of this we choose not put a explicit return too.
        """
        return CapacitorsS._name(self)

    @name.setter
    def name(self, argument: str):
        CapacitorsS._name_write(self, argument)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings with all Capacitor names in the circuit."""
        return CapacitorsV._names(self)

    @property
    def states(self) -> List[int]:
        """Gets a variant array of integers [0..numsteps-1] indicating the state of each step.
                If value is -1 and error has occurred.

        Sets a variant array of integers [0..numsteps-1] indicating the state of each step. If value is -1 and
        error has occurred.
        """
        return CapacitorsV._states_read(self)

    @states.setter
    def states(self, argument: List[int]):
        CapacitorsV._states_write(self, argument)

    def first(self) -> int:
        """Sets the first capacitor active. Returns 0 if no more."""
        return CapacitorsI._first(self)

    def next(self) -> int:
        """Sets the next capacitor active. Returns 0 if no more."""
        return CapacitorsI._next(self)

    def add_step(self) -> int:
        """Adds one step of the capacitor if available. If successful returns 1."""
        return CapacitorsI._add_step(self)

    def subtract_step(self) -> int:
        """Subtracts one step of the capacitor if available. If no more steps, returns 0."""
        return CapacitorsI._subtract_step(self)

