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
        return RelaysV.all_names(self)

    @property
    def name(self):
        return RelaysS.name(self)

    @name.setter
    def name(self, argument):
        RelaysS.name_write(self, argument)

    @property
    def monitored_obj(self):
        return RelaysS.monitored_obj(self)

    @monitored_obj.setter
    def monitored_obj(self, argument):
        RelaysS.monitored_obj_write(self, argument)

    @property
    def switched_obj(self):
        return RelaysS.switched_obj(self)

    @switched_obj.setter
    def switched_obj(self, argument):
        RelaysS.switched_obj_write(self, argument)

    @property
    def count(self) -> int:
        return RelaysI.count(self)

    @property
    def first(self) -> int:
        return RelaysI.first(self)

    @property
    def next(self) -> int:
        return RelaysI.next(self)

    @property
    def monitored_term(self) -> int:
        return RelaysI.monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, argument):
        RelaysI.monitored_term_write(self, argument)

    @property
    def switched_term(self) -> int:
        return RelaysI.switched_term(self)

    @switched_term.setter
    def switched_term(self, argument):
        RelaysI.switched_term_write(self, argument)

    @property
    def idx(self) -> int:
        return RelaysI.idx(self)

    @idx.setter
    def idx(self, argument):
        RelaysI.idx_write(self, argument)

