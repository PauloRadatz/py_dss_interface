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
        """Gets the present value of the Irradiance property in W/sq-m.
        Sets the present value of the Irradiance property in W/sq-m."""
        return PVSystemsF._irradiance_read(self)

    @irradiance.setter
    def irradiance(self, argument: float):
        PVSystemsF._irradiance_write(self, argument)

    @property
    def kw_output(self) -> float:
        """Gets the kW output."""
        return PVSystemsF._kw(self)

    @property
    def kvar(self) -> float:
        """Gets the kvar value.
        Sets the kvar value."""
        return PVSystemsF._kvar_read(self)

    @kvar.setter
    def kvar(self, argument: float):
        PVSystemsF._kvar_write(self, argument)

    @property
    def pf(self) -> float:
        """Gets the power factor value.
        Sets the power factor value."""
        return PVSystemsF._pf_read(self)

    @pf.setter
    def pf(self, argument: float):
        PVSystemsF._pf_write(self, argument)

    @property
    def kva(self) -> float:
        """Gets the rated kVA of the PVSystem.
        Sets the rated kVA of the PVSystem."""
        return PVSystemsF._kva_rated_read(self)

    @kva.setter
    def kva(self, argument: float):
        PVSystemsF._kva_rated_write(self, argument)

    @property
    def pmpp(self) -> float:
        """Gets the Pmpp.
        Sets the Pmpp."""
        return PVSystemsF._pmpp_read(self)

    @pmpp.setter
    def pmpp(self, argument: float):
        PVSystemsF._pmpp_write(self, argument)

    @property
    def count(self) -> int:
        """Returns the number of PVSystem objects currently defined in the active circuit."""
        return PVSystemsI._count(self)

    def first(self) -> int:
        """Sets the first PVSystem to be active; returns 0 if none."""
        return PVSystemsI._first(self)

    def next(self) -> int:
        """Sets the next PVSystem to be active; returns 0 if none."""
        return PVSystemsI._next(self)

    @property
    def idx(self) -> int:
        """Gets the active PVSystem by index; 1..Count.
        Sets the active PVSystem by index; 1..Count.."""
        return PVSystemsI._idx_read(self)

    @idx.setter
    def idx(self, argument: int):
        PVSystemsI._idx_write(self, argument)

    @property
    def name(self) -> str:
        """Gets the name of the active PVSystem.
        Sets the name of the active PVSystem."""
        return PVSystemsS._name_read(self)

    @name.setter
    def name(self, argument: str):
        PVSystemsS._name_write(self, argument)

    @property
    def names(self) -> List[str]:
        """Gets the variant array of string containing names of all PVSystems in the circuit."""
        return PVSystemsV._names(self)
