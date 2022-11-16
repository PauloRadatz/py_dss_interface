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
        return ISourcesF._amps_read(self)

    @amps.setter
    def amps(self, argument: float):
        ISourcesF._amps_write(self, argument)

    @property
    def angle_deg(self) -> float:
        return ISourcesF._angle_deg_read(self)

    @angle_deg.setter
    def angle_deg(self, argument: float):
        ISourcesF._angle_deg_write(self, argument)

    @property
    def frequency(self) -> float:
        return ISourcesF._frequency_read(self)

    @frequency.setter
    def frequency(self, argument: float):
        ISourcesF._frequency_write(self, argument)

    @property
    def count(self) -> int:
        return ISourcesI._count(self)

    def first(self) -> int:
        return ISourcesI._first(self)

    def next(self) -> int:
        return ISourcesI._next(self)

    @property
    def name(self) -> str:
        return ISourcesS._name_read(self)

    @name.setter
    def name(self, value: str):
        ISourcesS._name_write(self, value)

    @property
    def names(self) -> List[str]:
        return ISourcesV._names(self)
