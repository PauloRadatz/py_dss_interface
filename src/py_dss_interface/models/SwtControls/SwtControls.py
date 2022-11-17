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

    @property
    def delay(self) -> float:
        return SwtControlsF._delay_read(self)

    @delay.setter
    def delay(self, value: float):
        SwtControlsF._delay_write(self, value)

    def first(self) -> int:
        return SwtControlsI._first(self)

    def next(self) -> int:
        return SwtControlsI._next(self)

    @property
    def action(self) -> int:
        return SwtControlsI._action_read(self)

    @action.setter
    def action(self, value: int):
        SwtControlsI._action_write(self, value)

    @property
    def is_locked(self) -> int:
        return SwtControlsI._is_locked_read(self)

    @is_locked.setter
    def is_locked(self, value: int):
        SwtControlsI._is_locked_write(self, value)

    @property
    def switched_term(self) -> int:
        return SwtControlsI._switched_term_read(self)

    @switched_term.setter
    def switched_term(self, value: int):
        SwtControlsI._switched_term_write(self, value)

    @property
    def count(self) -> int:
        return SwtControlsI._count(self)

    @property
    def name(self) -> str:
        return SwtControlsS._name_read(self)

    @name.setter
    def name(self, value: str):
        SwtControlsS._name_write(self, value)

    @property
    def switched_obj(self) -> str:
        return SwtControlsS._switched_obj_read(self)

    @switched_obj.setter
    def switched_obj(self, value: str):
        SwtControlsS._switched_obj_write(self, value)

    @property
    def names(self) -> List[str]:
        return SwtControlsV._names(self)
