# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.PVSystems.PVSystemsF import PVSystemsF
from py_dss_interface.models.PVSystems.PVSystemsI import PVSystemsI
from py_dss_interface.models.PVSystems.PVSystemsS import PVSystemsS
from py_dss_interface.models.PVSystems.PVSystemsV import PVSystemsV
from typing import List


class PVSystems(PVSystemsV, PVSystemsS, PVSystemsI, PVSystemsF):
    """
    This interface implements the PVSystems (IPVSystems) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: PVSystemsV, PVSystemsS, PVSystemsI, PVSystemsF.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def irradiance(self) -> float:
        return PVSystemsF._irradiance_read(self)

    @irradiance.setter
    def irradiance(self, argument: float):
        PVSystemsF._irradiance_write(self, argument)

    @property
    def kw_output(self) -> float:
        return PVSystemsF._kw(self)

    @property
    def kvar(self) -> float:
        return PVSystemsF._kvar_read(self)

    @kvar.setter
    def kvar(self, argument: float):
        PVSystemsF._kvar_write(self, argument)

    @property
    def pf(self) -> float:
        return PVSystemsF._pf_read(self)

    @pf.setter
    def pf(self, argument: float):
        PVSystemsF._pf_write(self, argument)

    @property
    def kva_rated(self) -> float:
        return PVSystemsF._kva_rated_read(self)

    @kva_rated.setter
    def kva_rated(self, argument: float):
        PVSystemsF._kva_rated_write(self, argument)

    @property
    def pmpp(self) -> float:
        return PVSystemsF._pmpp_read(self)

    @pmpp.setter
    def pmpp(self, argument: float):
        PVSystemsF._pmpp_write(self, argument)

    @property
    def count(self) -> int:
        return PVSystemsI._count(self)

    def first(self) -> int:
        return PVSystemsI._first(self)

    def next(self) -> int:
        return PVSystemsI._next(self)

    @property
    def idx(self) -> int:
        return PVSystemsI._idx_read(self)

    @idx.setter
    def idx(self, argument: int):
        PVSystemsI._idx_write(self, argument)

    @property
    def name(self) -> str:
        return PVSystemsS._name_read(self)

    @name.setter
    def name(self, argument: str):
        PVSystemsS._name_write(self, argument)

    @property
    def names(self) -> List[str]:
        return PVSystemsV._names(self)
