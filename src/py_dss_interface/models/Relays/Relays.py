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
        """Gets number of Relays in active circuit."""
        return RelaysI._count(self)

    def first(self) -> int:
        """Sets first relay active. If none, returns 0."""
        return RelaysI._first(self)

    def next(self) -> int:
        """Sets next relay active. If none, returns 0."""
        return RelaysI._next(self)

    def open(self) -> int:
        """This parameter opens recloser's controlled element and lock out the relay."""
        return RelaysI._open(self)

    def close(self) -> int:
        """This parameter closes the switched object controlled by the relay. Resets relay to first operation."""
        return RelaysI._close(self)

    def reset(self) -> int:
        """This parameter resets the relay to its normal state. If open, lock out the relay. If closed, resets relay to first operation."""
        return RelaysI._reset(self)

    @property
    def state(self) -> str:
        """This property gets the present state of relay.
        This property sets the present state of relay. If set to open, open relay's controlled element and lock out the relay. If set to close, close relay's controlled element and resets relay to first operation."""
        return RelaysS._state_read(self)

    @state.setter
    def state(self, argument: str):
        RelaysS._state_write(self, argument)

    @property
    def normal_state(self) -> str:
        """This property gets the normal state (the state for which the active relay will be forced into at the beginning of the simulation) for the active relay.
        This property sets the normal state (the state for which the active relay will be forced into at the beginning of the simulation) for the active relay."""
        return RelaysS._normal_state_read(self)

    @normal_state.setter
    def normal_state(self, argument: str):
        RelaysS._normal_state_write(self, argument)

    @property
    def monitored_term(self) -> int:
        """Gets the number of terminal of monitored element that this relay is monitoring.
        Sets the number of terminal of monitored element that this relay is monitoring."""
        return RelaysI._monitored_term_read(self)

    @monitored_term.setter
    def monitored_term(self, arg: int):
        RelaysI._monitored_term_write(self, arg)

    @property
    def switched_term(self) -> int:
        """Gets the number of terminal of the switched object that will be opened when the relay trips.
        Sets the number of terminal of the switched object that will be opened when the relay trips."""
        return RelaysI._switched_term_read(self)

    @switched_term.setter
    def switched_term(self, arg: int):
        RelaysI._switched_term_write(self, arg)

    @property
    def idx(self) -> int:
        """Gets the active relay by index into the Relay list. 1..Count.
        Sets the active relay by index into the Relay list. 1..Count."""
        return RelaysI._idx_read(self)

    @idx.setter
    def idx(self, arg: int):
        RelaysI._idx_write(self, arg)

    @property
    def name(self):
        """Gets the name of the active Relay.
        Sets the name of the active Relay."""
        return RelaysS._name_read(self)

    @name.setter
    def name(self, arg: str):
        RelaysS._name_write(self, arg)

    @property
    def monitored_obj(self):
        """Gets the full name of the object this relay is monitoring.
        Sets the full name of the object this relay is monitoring."""
        return RelaysS._monitored_obj_read(self)

    @monitored_obj.setter
    def monitored_obj(self, arg: str):
        RelaysS._monitored_obj_write(self, arg)

    @property
    def switched_obj(self):
        """Gets the full name of element that will switched when relay trips.
        Sets the full name of element that will switched when relay trips."""
        return RelaysS._switched_obj_read(self)

    @switched_obj.setter
    def switched_obj(self, arg: str):
        RelaysS._switched_obj_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings containing names of all relay elements."""
        return RelaysV._names(self)

