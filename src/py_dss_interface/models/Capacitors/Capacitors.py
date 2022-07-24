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
        return CapacitorsI._num_steps(self)

    @num_steps.setter
    def num_steps(self, argument: int):
        CapacitorsI._num_steps_write(self, argument)

    @property
    def is_delta(self) -> int:
        return CapacitorsI.is_delta(self)

    @is_delta.setter
    def is_delta(self, argument: int):
        CapacitorsI._is_delta_write(self, argument)

    @property
    def first(self) -> int:
        return CapacitorsI._first(self)

    @property
    def next(self) -> int:
        return CapacitorsI._next(self)

    @property
    def count(self) -> int:
        return CapacitorsI._count(self)

    @property
    def add_step(self) -> int:
        return CapacitorsI._add_step(self)

    @property
    def subtract_step(self) -> int:
        return CapacitorsI._subtract_step(self)

    @property
    def available_steps(self) -> int:
        return CapacitorsI._available_steps(self)

    @property
    def open_all_steps(self) -> int:
        return CapacitorsI._open_all_steps(self)

    @property
    def close_all_steps(self) -> int:
        return CapacitorsI._close_all_steps(self)

    @property
    def kv(self) -> float:
        return CapacitorsF._kv(self)

    @kv.setter
    def kv(self, argument: float):
        CapacitorsF._kv_write(self, argument)

    @property
    def kvar(self) -> float:
        return CapacitorsF._kvar(self)

    @kvar.setter
    def kvar(self, argument: float):
        CapacitorsF._kvar_write(self, argument)

    @property
    def name(self) -> str:
        return CapacitorsS._name(self)

    @name.setter
    def name(self, argument: str):
        CapacitorsS._name_write(self, argument)

    @property
    def names(self) -> List[str]:
        return CapacitorsV._names(self)

    @property
    def states(self) -> List[int]:
        return CapacitorsV._states(self)

    @states.setter
    def states(self, values):
        dss, argument = values
        CapacitorsV._states_write(self, dss, argument)
