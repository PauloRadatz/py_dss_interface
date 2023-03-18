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

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def close(self) -> int:
        """Closing of fuse."""
        return FusesI._close(self)

    @property
    def count(self) -> int:
        """Returns the number of Fuses objects currently defined in the active circuit."""
        return FusesI._count(self)

    @property
    def delay(self) -> float:
        return FusesF._delay(self)

    @delay.setter
    def delay(self, value: float):
        """Gets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default
                is 0.
        Sets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default
                is 0. """
        FusesF._delay_write(self, value)

    def first(self) -> int:
        """Sets the first Fuse to be the active Fuse. Returns 0 if none."""
        return FusesI._first(self)

    @property
    def idx(self) -> int:
        """Gets the active fuse by index into the list of fuses. 1 based: 1..count.
        Sets the active fuse by index into the list of fuses. 1 based: 1..count."""
        return FusesI._idx(self)

    @idx.setter
    def idx(self, value: int):
        FusesI._idx_write(self, value)

    @property
    def is_blown(self) -> int:
        """Returns the current state of the fuses. TRUE (1) if any on any phase is blown. Else FALSE (0)."""
        return FusesI._is_blown(self)

    @property
    def monitored_obj(self) -> str:
        """Gets the name of the Monitored Object by the active fuse.
        Sets the name of the Monitored Object by the active fuse."""
        return FusesS._monitored_obj(self)

    @monitored_obj.setter
    def monitored_obj(self, value: str):
        FusesS._monitored_obj_write(self, value)

    @property
    def monitored_term(self) -> int:
        """Gets the terminal number to switch the fuse is connected.
        Sets the terminal number to switch the fuse is connected."""
        return FusesI._monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, value: int):
        FusesI._monitored_term_write(self, value)

    @property
    def name(self) -> str:
        """Gets the name of the active fuse.
        Sets the name of the active fuse."""
        return FusesS._name(self)

    @name.setter
    def name(self, value: str):
        FusesS._name_write(self, value)

    @property
    def names(self) -> List[str]:
        """Gets the variant array of string containing names of all fuses in the circuit."""
        return FusesV._names(self)

    def next(self) -> int:
        """Sets the next Fuse to be the active Fuse. Returns 0 if none."""
        return FusesI._next(self)

    @property
    def normal(self) -> List[str]:
        """Gets a variant array of strings[0..Nphases-1] indicating the normal state for all phases of the active fuse.
                        If value is -1 an error has occurred.
        Sets a variant array of strings [0..Nphases-1] indicating the state for all phases of the active fuse.
                        If value is -1 an error has occurred."""
        return FusesV._normal(self)

    @normal.setter
    def normal(self, value: List[str]):
        FusesV._normal_write(self, value)

    @property
    def num_phases(self) -> int:
        """Gets the number of phases of the active fuse."""
        return FusesI._num_phases(self)

    @property
    def open(self) -> int:
        """Opening of fuse."""
        return FusesI._open(self)

    @property
    def rated_current(self) -> float:
        """Gets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0, Multiply current values of
                TCC curve by this to get actual amps.
        Sets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0, Multiply current values of
                TCC curve by this to get actual amps. """
        return FusesF._rated_current(self)

    @rated_current.setter
    def rated_current(self, value: float):
        FusesF._rated_current_write(self, value)

    def reset(self):
        """Resets the state of the fuse object to the normal state."""
        return FusesI._reset(self)

    @property
    def state(self) -> List[str]:
        """Gets a variant array of strings[0..Nphases-1] indicating the present state for all phases of the active fuse.
                If value is -1 an error has occurred.
        Sets a variant array of strings [0..Nphases-1] indicating the state for all phases of the active fuse.
                If value is -1 an error has occurred."""
        return FusesV._state(self)

    @state.setter
    def state(self, value: List[str]):
        FusesV._state_write(self, value)

    @property
    def switched_obj(self) -> str:
        """Gets the full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj.
        Sets the full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj."""
        return FusesS._switched_obj(self)

    @switched_obj.setter
    def switched_obj(self, value: str):
        FusesS._switched_obj_write(self, value)

    @property
    def switched_term(self) -> int:
        """Gets the terminal number of the terminal containing the switch controlled by the fuse.
        Sets the terminal number of the terminal containing the switch controlled by the fuse."""
        return FusesI._switched_term(self)

    @switched_term.setter
    def switched_term(self, value: int):
        FusesI._switched_term_write(self, value)

    @property
    def tcc_curve(self) -> str:
        """Gets the name of the TCCcurve object that determines fuse blowing.
        Sets the name of the TCCcurve object that determines fuse blowing."""
        return FusesS._tcc_curve(self)

    @tcc_curve.setter
    def tcc_curve(self, value: str):
        FusesS._tcc_curve_write(self, value)
