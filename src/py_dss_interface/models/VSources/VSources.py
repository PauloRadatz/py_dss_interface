# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.VSources.VSourcesF import VSourcesF
from py_dss_interface.models.VSources.VSourcesI import VSourcesI
from py_dss_interface.models.VSources.VSourcesS import VSourcesS
from py_dss_interface.models.VSources.VSourcesV import VSourcesV
from typing import List


class VSources(VSourcesS, VSourcesV, VSourcesI, VSourcesF):
    """
    This interface implements the Vsources (IVSources) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: VSourcesS, VSourcesV, VSourcesI, VSourcesF.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def base_kv(self) -> float:
        """Gets the source voltage in kV.
        Sets the source voltage in kV."""
        return VSourcesF._base_kv_read(self)

    @base_kv.setter
    def base_kv(self, value: float):
        VSourcesF._base_kv_write(self, value)

    @property
    def pu(self) -> float:
        """Gets the source voltage in pu.
        Sets the source voltage in pu."""
        return VSourcesF._pu_read(self)

    @pu.setter
    def pu(self, value: float):
        VSourcesF._pu_write(self, value)

    @property
    def angle_deg(self) -> float:
        """Gets the source phase angle of first phase in degrees.
        Sets the source phase angle of first phase in degrees."""
        return VSourcesF._angle_deg_read(self)

    @angle_deg.setter
    def angle_deg(self, value: float):
        VSourcesF._angle_deg_write(self, value)

    @property
    def frequency(self) -> float:
        """Gets the source frequency in Hz.
        Sets the source frequency in Hz."""
        return VSourcesF._frequency_read(self)

    @frequency.setter
    def frequency(self, value: float):
        VSourcesF._frequency_write(self, value)

    @property
    def count(self) -> int:
        """Returns the number of VSource objects currently defined in the active circuit."""
        return VSourcesI._count(self)

    def first(self) -> int:
        """Sets the first VSource to be active; returns 0 if none."""
        return VSourcesI._first(self)

    def next(self) -> int:
        """Sets the next VSource to be active; returns 0 if none."""
        return VSourcesI._next(self)

    @property
    def phases(self) -> int:
        """Gets the number of phases of the active VSource.
        Sets the number of phases of the active VSource."""
        return VSourcesI._phases_read(self)

    @phases.setter
    def phases(self, phases: int):
        VSourcesI._phases_write(self, phases)

    @property
    def name(self) -> str:
        """Gets the name of the active VSource.
        Sets the name of the active VSource."""
        return VSourcesS._name_read(self)

    @name.setter
    def name(self, name: str):
        VSourcesS._name_write(self, name)

    @property
    def names(self) -> List[str]:
        """Gets the name of the active VSource."""
        return VSourcesV._names(self)










