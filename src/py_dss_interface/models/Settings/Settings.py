# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Settings.SettingsF import SettingsF
from py_dss_interface.models.Settings.SettingsI import SettingsI
from py_dss_interface.models.Settings.SettingsS import SettingsS
from py_dss_interface.models.Settings.SettingsV import SettingsV
from typing import List


class Settings(SettingsS, SettingsF, SettingsI, SettingsV):
    """
    This interface implements the Settings (ISettings) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: SettingsS, SettingsF, SettingsI, SettingsV.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def allocation_factors(self) -> float:
        """Sets all load allocation factors for all loads defined by XFKVA property to this value."""
        return SettingsF._allocation_factors(self)

    @property
    def norm_vmin_pu(self) -> float:
        """Gets the per unit minimum voltage for Normal conditions.

        Sets the per unit minimum voltage for Normal conditions."""
        return SettingsF._norm_vmin_pu_read(self)

    @norm_vmin_pu.setter
    def norm_vmin_pu(self, arg: float):
        SettingsF._norm_vmin_pu_write(self, arg)

    @property
    def norm_vmax_pu(self) -> float:
        """Gets the per unit maximum voltage for Normal conditions.

        Sets the per unit maximum voltage for Normal conditions."""
        return SettingsF._norm_vmax_pu_read(self)

    @norm_vmax_pu.setter
    def norm_vmax_pu(self, arg: float):
        SettingsF._norm_vmax_pu_write(self, arg)

    @property
    def emerg_vmin_pu(self) -> float:
        """Gets the per unit minimum voltage for Emergency conditions.

        Sets the per unit minimum voltage for Emergency conditions."""
        return SettingsF._emerg_vmin_pu_read(self)

    @emerg_vmin_pu.setter
    def emerg_vmin_pu(self, arg: float):
        SettingsF._emerg_vmin_pu_write(self, arg)

    @property
    def emerg_vmax_pu(self) -> float:
        """Gets the per unit maximum voltage for Emergency conditions.

        Sets the per unit maximum voltage for Emergency conditions."""
        return SettingsF._emerg_vmax_pu_read(self)

    @emerg_vmax_pu.setter
    def emerg_vmax_pu(self, arg: float):
        SettingsF._emerg_vmax_pu_write(self, arg)

    @property
    def ue_weight(self) -> float:
        """Gets the weighting factor applied to UE register values.

        Sets the weighting factor applied to UE register values."""
        return SettingsF._ue_weight_read(self)

    @ue_weight.setter
    def ue_weight(self, arg: float):
        SettingsF._ue_weight_write(self, arg)

    @property
    def loss_weight(self) -> float:
        """Gets the weighting factor applied to Loss register values.

        Sets the weighting factor applied to Loss register values."""
        return SettingsF._loss_weight_read(self)

    @loss_weight.setter
    def loss_weight(self, arg: float):
        SettingsF._loss_weight_write(self, arg)

    @property
    def price_signal(self) -> float:
        """Gets the price signal for the circuit.

        Sets the price signal for the circuit."""
        return SettingsF._price_signal_read(self)

    @price_signal.setter
    def price_signal(self, arg: float):
        SettingsF._price_signal_write(self, arg)

    @property
    def allow_duplicates(self) -> int:
        """Gets if OpenDSS allows duplicate names of objects: {1 allow, 0 not allow}.

        Sets if OpenDSS allows duplicate names of objects: {1 allow, 0 not allow}."""
        return SettingsI._allow_duplicates_read(self)

    @allow_duplicates.setter
    def allow_duplicates(self, arg: int):
        SettingsI._allow_duplicates_write(self, arg)

    @property
    def zone_lock(self) -> int:
        """Gets the status of Lock zones on energy meters to prevent rebuilding if a circuit
                change occurs: {1= true, 0= False}.

        Sets the status of Lock zones on energy meters to prevent rebuilding if a circuit change occurs: {1= true,
        0= False}. """
        return SettingsI._zone_lock_read(self)

    @zone_lock.setter
    def zone_lock(self, arg: int):
        SettingsI._zone_lock_write(self, arg)

    @property
    def ckt_model(self) -> int:
        """Gets {dssMultiphase* | dssPositiveSeq} Indicate if the circuit model is positive sequence.

        Sets {dssMultiphase* | dssPositiveSeq} Indicate if the circuit model is positive sequence."""
        return SettingsI._ckt_model_read(self)

    @ckt_model.setter
    def ckt_model(self, arg: int):
        SettingsI._ckt_model_write(self, arg)

    @property
    def trapezoidal(self) -> int:
        """Gets {True (1) | False (0)} value of trapezoidal integration flag in Energy Meters.

        Sets {True (1) | False (0)} value of trapezoidal integration flag in Energy Meters."""
        return SettingsI._trapezoidal_read(self)

    @trapezoidal.setter
    def trapezoidal(self, arg: int):
        SettingsI._trapezoidal_write(self, arg)

    @property
    def auto_bus_list(self) -> str:
        """Gets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode.

        Sets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        return SettingsS._auto_bus_list_read(self)

    @auto_bus_list.setter
    def auto_bus_list(self, arg: str):
        SettingsS._auto_bus_list_write(self, arg)

    @property
    def price_curve(self) -> str:
        """Gets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
                etc.

        Sets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        return SettingsS._price_curve_read(self)

    @price_curve.setter
    def price_curve(self, arg: str):
        SettingsS._price_curve_write(self, arg)

    @property
    def ue_regs(self) -> List[int]:
        """Gets the array of Integers defining Energy Meter registers to use for computing UE.

        Sets the array of Integers defining Energy Meter registers to use for computing UE."""
        return SettingsV._ue_regs_read(self)

    @ue_regs.setter
    def ue_regs(self, arg: List[int]):
        SettingsV._ue_regs_write(self, arg)

    @property
    def loss_regs(self) -> List[int]:
        """Gets the array of Integers defining Energy Meter registers to use for computing Losses.

        Sets the array of Integers defining Energy Meter registers to use for computing Losses."""
        return SettingsV._loss_regs_read(self)

    @loss_regs.setter
    def loss_regs(self, arg: List[int]):
        SettingsV._loss_regs_write(self, arg)

    @property
    def voltage_bases(self) -> List[float]:
        """Gets the array of doubles defining the legal voltage bases in kV L-L.

        Sets the array of doubles defining the legal voltage bases in kV L-L."""
        return SettingsV._voltage_bases_read(self)

    @voltage_bases.setter
    def voltage_bases(self, arg: List[float]):
        SettingsV._voltage_bases_write(self, arg)
