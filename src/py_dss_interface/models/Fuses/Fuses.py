# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Fuses.FusesF import FusesF
from py_dss_interface.models.Fuses.FusesI import FusesI
from py_dss_interface.models.Fuses.FusesS import FusesS
from py_dss_interface.models.Fuses.FusesV import FusesV


class Fuses(FusesI, FusesS, FusesF, FusesV):
    """" This interface implements the Fuses (IFuses) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: FusesI, FusesS, FusesF, FusesV """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    def close(self) -> int:
        return FusesI._close(self)

    @property
    def count(self) -> int:
        return FusesI._count(self)

    @property
    def delay(self) -> float:
        return FusesF._delay(self)

    @delay.setter
    def delay(self, value: float):
        FusesF._delay_write(self, value)

    def first(self) -> int:
        return FusesI._first(self)

    @property
    def idx(self) -> int:
        return FusesI._idx(self)

    @idx.setter
    def idx(self, value: int):
        FusesI._idx_write(self, value)

    @property
    def is_blown(self) -> int:
        return FusesI._is_blown(self)

    @property
    def monitored_obj(self) -> str:
        return FusesS._monitored_obj(self)

    @monitored_obj.setter
    def monitored_obj(self, value: str):
        FusesS._monitored_obj_write(self, value)

    @property
    def monitored_term(self) -> int:
        return FusesI._monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, value: int):
        FusesI._monitored_term_write(self, value)

    @property
    def name(self) -> str:
        return FusesS._name(self)

    @name.setter
    def name(self, value: str):
        FusesS._name_write(self, value)

    @property
    def names(self) -> List[str]:
        return FusesV._names(self)

    @property
    def next(self) -> int:
        return FusesI._next(self)

    @property
    def normal(self):
        return FusesV._normal(self)

    @normal.setter
    def normal(self, value):
        FusesV._normal_write(self, value)

    @property
    def num_phases(self) -> int:
        return FusesI._num_phases(self)

    @property
    def open(self) -> int:
        return FusesI._open(self)

    @property
    def rated_current(self) -> float:
        return FusesF._rated_current(self)

    @rated_current.setter
    def rated_current(self, value: float):
        FusesF._rated_current_write(self, value)

    @property
    def reset(self):
        return FusesI._reset(self)

    @property
    def state(self):
        return FusesV._state(self)

    @state.setter
    def state(self, value):
        FusesV._state_write(self, value)

    @property
    def switched_obj(self) -> str:
        return FusesS._switched_obj(self)

    @switched_obj.setter
    def switched_obj(self, value: str):
        FusesS._switched_obj_write(self, value)

    @property
    def switched_term(self) -> int:
        return FusesI._switched_term(self)

    @switched_term.setter
    def switched_term(self, value: int):
        FusesI._switched_term_write(self, value)

    @property
    def tcc_curve(self) -> str:
        return FusesS._tcc_curve(self)

    @tcc_curve.setter
    def tcc_curve(self, value: str):
        FusesS._tcc_curve_write(self, value)
