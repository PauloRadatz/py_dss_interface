# -*- coding: iso-8859-15 -*-

from py_dss_interface.models.Reclosers.ReclosersF import ReclosersF
from py_dss_interface.models.Reclosers.ReclosersI import ReclosersI
from py_dss_interface.models.Reclosers.ReclosersS import ReclosersS
from py_dss_interface.models.Reclosers.ReclosersV import ReclosersV


class Reclosers(ReclosersI, ReclosersV, ReclosersS, ReclosersF):
    """
    This interface implements the Reclosers (IReclosers) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ReclosersI, ReclosersV, ReclosersS, ReclosersF._
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def phase_trip(self) -> float:
        return ReclosersF._phase_trip(self)

    @phase_trip.setter
    def phase_trip(self, argument):
        ReclosersF._phase_trip_write(self, argument)

    @property
    def phase_inst(self) -> float:
        return ReclosersF._phase_inst(self)

    @phase_inst.setter
    def phase_inst(self, argument):
        ReclosersF._phase_inst_write(self, argument)

    @property
    def ground_trip(self):
        return ReclosersF._ground_trip(self)

    @ground_trip.setter
    def ground_trip(self, argument):
        ReclosersF._ground_trip_write(self, argument)

    @property
    def ground_inst(self) -> float:
        return ReclosersF._ground_inst(self)

    @ground_inst.setter
    def ground_inst(self, argument):
        ReclosersF._ground_inst_write(self, argument)

    @property
    def count(self) -> int:
        return ReclosersI._count(self)

    @property
    def first(self) -> int:
        return ReclosersI._first(self)

    @property
    def next(self) -> int:
        return ReclosersI._next(self)

    @property
    def monitored_term(self) -> int:
        return ReclosersI._monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, argument):
        ReclosersI._monitored_term_write(self, argument)

    @property
    def switched_term(self) -> int:
        return ReclosersI._switched_term(self)

    @switched_term.setter
    def switched_term(self, argument):
        ReclosersI._switched_term_write(self, argument)

    @property
    def num_fast(self) -> int:
        return ReclosersI._num_fast(self)

    @num_fast.setter
    def num_fast(self, argument):
        ReclosersI._num_fast_write(self, argument)

    @property
    def shots(self) -> int:
        return ReclosersI._shots(self)

    @shots.setter
    def shots(self, argument):
        ReclosersI._shots_write(self, argument)

    @property
    def open(self) -> int:
        return ReclosersI._open(self)

    @property
    def close(self) -> int:
        return ReclosersI._close(self)

    @property
    def idx(self) -> int:
        return ReclosersI._idx(self)

    @idx.setter
    def idx(self, argument):
        ReclosersI._idx_write(self, argument)

    @property
    def name(self) -> str:
        return ReclosersS._name(self)

    @name.setter
    def name(self, argument):
        ReclosersS._name_write(self, argument)

    @property
    def monitored_obj(self) -> str:
        return ReclosersS._monitored_obj(self)

    @monitored_obj.setter
    def monitored_obj(self, argument):
        ReclosersS._monitored_obj_write(self, argument)

    @property
    def switched_obj(self) -> str:
        return ReclosersS._switched_obj(self)

    @switched_obj.setter
    def switched_obj(self, argument):
        ReclosersS._switched_obj_write(self, argument)

    @property
    def all_names(self):
        return ReclosersV._all_names(self)

    @property
    def intervals(self):
        return ReclosersV._reclose_intervals(self)
