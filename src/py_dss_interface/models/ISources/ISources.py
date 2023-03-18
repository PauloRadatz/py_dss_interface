# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.ISources.ISourcesF import ISourcesF
from py_dss_interface.models.ISources.ISourcesI import ISourcesI
from py_dss_interface.models.ISources.ISourcesS import ISourcesS
from py_dss_interface.models.ISources.ISourcesV import ISourcesV
from typing import List


class ISources(ISourcesI, ISourcesF, ISourcesS, ISourcesV):
    """
    This interface implements the ISources (IIsources) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ISourcesI, ISourcesF, ISourcesS, ISourcesV.
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def amps(self) -> float:
        """Gets the magnitude of the Isource in Amps.
        Sets the magnitude of the Isource in Amps."""
        return ISourcesF._amps_read(self)

    @amps.setter
    def amps(self, argument: float):
        ISourcesF._amps_write(self, argument)

    @property
    def angle_deg(self) -> float:
        """Gets the phase angle of the Isource in degrees.
        Sets the phase angle of the Isource in degrees."""
        return ISourcesF._angle_deg_read(self)

    @angle_deg.setter
    def angle_deg(self, argument: float):
        ISourcesF._angle_deg_write(self, argument)

    @property
    def frequency(self) -> float:
        """Gets the frequency of the Isource in Hz.
        Sets the frequency of the Isource in Hz."""
        return ISourcesF._frequency_read(self)

    @frequency.setter
    def frequency(self, argument: float):
        ISourcesF._frequency_write(self, argument)

    @property
    def count(self) -> int:
        """Returns the number of Isource objects currently defined in the active circuit."""
        return ISourcesI._count(self)

    def first(self) -> int:
        """Sets the first ISource to be active; returns 0 if none."""
        return ISourcesI._first(self)

    def next(self) -> int:
        """Sets the next ISource to be active; returns 0 if none."""
        return ISourcesI._next(self)

    @property
    def name(self) -> str:
        """Gets the name of the active Isource object.
        Sets the name of the active Isource object."""
        return ISourcesS._name_read(self)

    @name.setter
    def name(self, value: str):
        ISourcesS._name_write(self, value)

    @property
    def names(self) -> List[str]:
        """Gets the variant array of string containing names of all ISources in the circuit."""
        return ISourcesV._names(self)
