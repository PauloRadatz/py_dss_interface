# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Relays.RelaysI import RelaysI
from py_dss_interface.models.Relays.RelaysS import RelaysS
from py_dss_interface.models.Relays.RelaysV import RelaysV
from typing import List


class Relays(RelaysS, RelaysV, RelaysI):
    """
    This interface implements the Relays (IRelays) interface of OpenDSS by declaring 3 procedures for accessing the
    different properties included in this interface: RelaysS, RelaysV, RelaysI.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def count(self) -> int:
        return RelaysI._count(self)

    def first(self) -> int:
        return RelaysI._first(self)

    def next(self) -> int:
        return RelaysI._next(self)

    @property
    def monitored_term(self) -> int:
        return RelaysI._monitored_term_read(self)

    @monitored_term.setter
    def monitored_term(self, arg: int):
        RelaysI._monitored_term_write(self, arg)

    @property
    def switched_term(self) -> int:
        return RelaysI._switched_term_read(self)

    @switched_term.setter
    def switched_term(self, arg: int):
        RelaysI._switched_term_write(self, arg)

    @property
    def idx(self) -> int:
        return RelaysI._idx_read(self)

    @idx.setter
    def idx(self, arg: int):
        RelaysI._idx_write(self, arg)

    @property
    def name(self):
        return RelaysS._name_read(self)

    @name.setter
    def name(self, arg: str):
        RelaysS._name_write(self, arg)

    @property
    def monitored_obj(self):
        return RelaysS._monitored_obj_read(self)

    @monitored_obj.setter
    def monitored_obj(self, arg: str):
        RelaysS._monitored_obj_write(self, arg)

    @property
    def switched_obj(self):
        return RelaysS._switched_obj_read(self)

    @switched_obj.setter
    def switched_obj(self, arg: str):
        RelaysS._switched_obj_write(self, arg)

    @property
    def names(self) -> List[str]:
        return RelaysV._names(self)

