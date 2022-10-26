# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Relays.RelaysI import RelaysI
from py_dss_interface.models.Relays.RelaysS import RelaysS
from py_dss_interface.models.Relays.RelaysV import RelaysV


class Relays(RelaysS, RelaysV, RelaysI):
    """
    This interface implements the Relays (IRelays) interface of OpenDSS by declaring 3 procedures for accessing the
    different properties included in this interface: RelaysS, RelaysV, RelaysI.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def all_names(self):
        return RelaysV._all_names(self)

    @property
    def name(self):
        return RelaysS._name(self)

    @name.setter
    def name(self, argument):
        RelaysS._name_write(self, argument)

    @property
    def monitored_obj(self):
        return RelaysS._monitored_obj(self)

    @monitored_obj.setter
    def monitored_obj(self, argument):
        RelaysS._monitored_obj_write(self, argument)

    @property
    def switched_obj(self):
        return RelaysS._switched_obj(self)

    @switched_obj.setter
    def switched_obj(self, argument):
        RelaysS._switched_obj_write(self, argument)

    @property
    def count(self) -> int:
        return RelaysI._count(self)

    @property
    def first(self) -> int:
        return RelaysI._first(self)

    @property
    def next(self) -> int:
        return RelaysI._next(self)

    @property
    def monitored_term(self) -> int:
        return RelaysI._monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, argument):
        RelaysI._monitored_term_write(self, argument)

    @property
    def switched_term(self) -> int:
        return RelaysI._switched_term(self)

    @switched_term.setter
    def switched_term(self, argument):
        RelaysI._switched_term_write(self, argument)

    @property
    def idx(self) -> int:
        return RelaysI._idx(self)

    @idx.setter
    def idx(self, argument):
        RelaysI._idx_write(self, argument)

