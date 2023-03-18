# -*- coding: iso-8859-15 -*-

from py_dss_interface.models.Reclosers.ReclosersF import ReclosersF
from py_dss_interface.models.Reclosers.ReclosersI import ReclosersI
from py_dss_interface.models.Reclosers.ReclosersS import ReclosersS
from py_dss_interface.models.Reclosers.ReclosersV import ReclosersV
from typing import List


class Reclosers(ReclosersI, ReclosersV, ReclosersS, ReclosersF):
    """
    This interface implements the Reclosers (IReclosers) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ReclosersI, ReclosersV, ReclosersS, ReclosersF._
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def phase_trip(self) -> float:
        """Gets the phase trip curve multiplier or actual amps.
        Sets the phase trip curve multiplier or actual amps."""
        return ReclosersF._phase_trip_read(self)

    @phase_trip.setter
    def phase_trip(self, argument):
        ReclosersF._phase_trip_write(self, argument)

    @property
    def phase_inst(self) -> float:
        """Gets the phase instantaneous curve multiplier or actual amps.
        Sets the phase instantaneous curve multiplier or actual amps."""
        return ReclosersF._phase_inst_read(self)

    @phase_inst.setter
    def phase_inst(self, argument):
        ReclosersF._phase_inst_write(self, argument)

    @property
    def ground_trip(self):
        """Gets the ground (3I0) trip multiplier or actual amps.
        Sets the ground (3I0) trip multiplier or actual amps."""
        return ReclosersF._ground_trip_read(self)

    @ground_trip.setter
    def ground_trip(self, argument):
        ReclosersF._ground_trip_write(self, argument)

    @property
    def ground_inst(self) -> float:
        """Gets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps.
        Sets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps."""
        return ReclosersF._ground_inst_read(self)

    @ground_inst.setter
    def ground_inst(self, argument):
        ReclosersF._ground_inst_write(self, argument)

    @property
    def count(self) -> int:
        """Gets number of Reclosers in active circuit."""
        return ReclosersI._count(self)

    def first(self) -> int:
        """Sets first recloser to be active Circuit Element. Returns 0 if none."""
        return ReclosersI._first(self)

    def next(self) -> int:
        """Sets next recloser to be active Circuit Element. Returns 0 if none."""
        return ReclosersI._next(self)

    @property
    def monitored_term(self) -> int:
        """Gets the terminal number of Monitored Object for the Recloser.
        Sets the terminal number of Monitored Object for the Recloser."""
        return ReclosersI._monitored_term_read(self)

    @monitored_term.setter
    def monitored_term(self, argument):
        ReclosersI._monitored_term_write(self, argument)

    @property
    def switched_term(self) -> int:
        """Gets the terminal of the controlled device being switched by the Recloser.
        Sets the terminal of the controlled device being switched by the Recloser."""
        return ReclosersI._switched_term_read(self)

    @switched_term.setter
    def switched_term(self, argument):
        ReclosersI._switched_term_write(self, argument)

    @property
    def num_fast(self) -> int:
        """Gets the number of fast shots.
        Sets the number of fast shots."""
        return ReclosersI._num_fast_read(self)

    @num_fast.setter
    def num_fast(self, argument):
        ReclosersI._num_fast_write(self, argument)

    @property
    def shots(self) -> int:
        """Gets the number of shots to lockout (fast + delayed).
        Sets the number of shots to lockout (fast + delayed)."""
        return ReclosersI._shots_read(self)

    @shots.setter
    def shots(self, argument):
        ReclosersI._shots_write(self, argument)

    def open(self) -> int:
        """Open recloser's controlled element and lock out the recloser."""
        return ReclosersI._open(self)

    def close(self) -> int:
        """Close the switched object controlled by the recloser. Resets recloser to first operation."""
        return ReclosersI._close(self)

    @property
    def idx(self) -> int:
        """Gets the active recloser by index into the recloser list. 1..Count.
        Sets the active recloser by index into the recloser list. 1..Count."""
        return ReclosersI._idx_read(self)

    @idx.setter
    def idx(self, argument: int):
        ReclosersI._idx_write(self, argument)

    @property
    def name(self) -> str:
        """Gets the name of the active Recloser Object.
        Sets the name of the active Recloser Object."""
        return ReclosersS._name_read(self)

    @name.setter
    def name(self, argument: str):
        ReclosersS._name_write(self, argument)

    @property
    def monitored_obj(self) -> str:
        """Gets the full name of object this Recloser is monitoring.
        Sets the full name of object this Recloser is monitoring."""
        return ReclosersS._monitored_obj_read(self)

    @monitored_obj.setter
    def monitored_obj(self, argument: str):
        ReclosersS._monitored_obj_write(self, argument)

    @property
    def switched_obj(self) -> str:
        """Gets the full name of the circuit element that is being switched by this Recloser.
        Sets the full name of the circuit element that is being switched by this Recloser."""
        return ReclosersS._switched_obj_read(self)

    @switched_obj.setter
    def switched_obj(self, argument: str):
        ReclosersS._switched_obj_write(self, argument)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings with names of all reclosers in active circuit.
        Gets a variant array of doubles: reclose intervals (s) between shots."""
        return ReclosersV._names(self)

    @property
    def intervals(self) -> List[float]:
        return ReclosersV._reclose_intervals(self)
