# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from dataclasses import dataclass
from typing import List

from py_dss_interface.models.ActiveClass.ActiveClassI import ActiveClassI
from py_dss_interface.models.ActiveClass.ActiveClassS import ActiveClassS
from py_dss_interface.models.ActiveClass.ActiveClassV import ActiveClassV


@dataclass
class ActiveClass(ActiveClassS, ActiveClassI, ActiveClassV):
    """This interface implements the ActiveClass (IActiveClass) interface of OpenDSS by declaring 3 procedures for
    accessing the different properties included in this interface: ActiveClassS, ActiveClassI, ActiveClassV. Ih the
    original paper Davis cited that are 4 procedures, but only 3 were described."""

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def first(self) -> int:
        return ActiveClassI._first(self)

    @property
    def next(self) -> int:
        return ActiveClassI._next(self)

    @property
    def num_elements(self) -> int:
        return ActiveClassI._num_elements(self)

    @property
    def count(self) -> int:
        return ActiveClassI._count(self)

    @property
    def name(self) -> str:
        return ActiveClassS._name(self)

    @name.setter
    def name(self, argument: str):
        ActiveClassS._name_write(self, argument)

    @property
    def class_name(self) -> str:
        return ActiveClassS._class_name(self)

    @property
    def parent_class_name(self) -> str:
        return ActiveClassS._parent_class_name(self)

    @property
    def names(self) -> List[str]:
        return ActiveClassV._names(self)
