# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.CapControls.CapControlsF import CapControlsF
from py_dss_interface.models.CapControls.CapControlsI import CapControlsI
from py_dss_interface.models.CapControls.CapControlsS import CapControlsS
from py_dss_interface.models.CapControls.CapControlsV import CapControlsV


class CapControls(CapControlsF, CapControlsI, CapControlsS, CapControlsV):
    """
    This interface implements the CapControls (ICapControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: CapControlsF, CapControlsI, CapControlsS,
    CapControlsV
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def mode(self) -> int:
        return CapControlsI._mode(self)

    @mode.setter
    def mode(self, argument: int):
        CapControlsI._mode_write(self, argument)

    @property
    def monitored_term(self) -> int:
        return CapControlsI._monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, values):
        dss, argument = values
        CapControlsI._monitored_term_write(self, dss, argument)

    @property
    def use_volt_override(self) -> int:
        return CapControlsI._use_volt_override(self)

    @use_volt_override.setter
    def use_volt_override(self, values):
        dss, argument = values
        CapControlsI._use_volt_override_write(self, dss, argument)

    @property
    def count(self) -> int:
        return CapControlsI._count(self)

    @property
    def names(self) -> List[str]:
        return CapControlsV._names(self)

    @property
    def ct_ratio(self) -> float:
        return CapControlsF._ct_ratio(self)

    @ct_ratio.setter
    def ct_ratio(self, argument: float):
        CapControlsF._ct_ratio_write(self, argument)

    @property
    def pt_ratio(self) -> float:
        return CapControlsF._pt_ratio(self)

    @pt_ratio.setter
    def pt_ratio(self, argument: float):
        CapControlsF._pt_ratio_write(self, argument)

    @property
    def on_setting(self) -> float:
        return CapControlsF._on_setting(self)

    @on_setting.setter
    def on_setting(self, argument: float):
        CapControlsF._on_setting_write(self, argument)

    @property
    def off_setting(self) -> float:
        return CapControlsF._off_setting(self)

    @off_setting.setter
    def off_setting(self, argument: float):
        CapControlsF._off_setting_write(self, argument)

    @property
    def vmax(self) -> float:
        return CapControlsF._vmax(self)

    @vmax.setter
    def vmax(self, argument: float):
        CapControlsF._vmax_write(self, argument)

    @property
    def vmin(self) -> float:
        return CapControlsF._vmin(self)

    @vmin.setter
    def vmin(self, argument: float):
        CapControlsF._vmin_write(self, argument)

    @property
    def delay(self) -> float:
        return CapControlsF._delay(self)

    @delay.setter
    def delay(self, argument: float):
        CapControlsF._delay_write(self, argument)

    @property
    def delay_off(self) -> float:
        return CapControlsF._delay_off(self)

    @delay_off.setter
    def delay_off(self, argument: float):
        CapControlsF._delay_off_write(self, argument)

    @property
    def dead_time(self) -> float:
        return CapControlsF._dead_time(self)

    @dead_time.setter
    def dead_time(self, argument: float):
        CapControlsF._dead_time_write(self, argument)

    @property
    def name(self) -> str:
        return CapControlsS._name(self)

    @name.setter
    def name(self, argument: str):
        CapControlsS._name_write(self, argument)

    @property
    def controlled_capacitor(self) -> str:
        return CapControlsS._controlled_capacitor(self)

    @controlled_capacitor.setter
    def controlled_capacitor(self, argument: str):
        CapControlsS._controlled_capacitor_write(self, argument)

    @property
    def monitored_object(self) -> str:
        return CapControlsS._monitored_object(self)

    @monitored_object.setter
    def monitored_object(self, argument: str):
        CapControlsS._monitored_object_write(self, argument)

    def first(self) -> int:
        return CapControlsI._first(self)

    def next(self) -> int:
        return CapControlsI._next(self)
