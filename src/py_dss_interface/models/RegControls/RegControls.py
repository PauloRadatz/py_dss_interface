# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.RegControls.RegControlsF import RegControlsF
from py_dss_interface.models.RegControls.RegControlsI import RegControlsI
from py_dss_interface.models.RegControls.RegControlsS import RegControlsS
from py_dss_interface.models.RegControls.RegControlsV import RegControlsV
from typing import List


class RegControls(RegControlsI, RegControlsF, RegControlsV, RegControlsS):
    """
    This interface implements the RegControls (IRegControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: RegControlsI, RegControlsF, RegControlsV,
    RegControlsS.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def ct_primary(self) -> float:
        return RegControlsF._ct_primary_read(self)

    @ct_primary.setter
    def ct_primary(self, arg: float):
        RegControlsF._ct_primary_write(self, arg)

    @property
    def pt_ratio(self) -> float:
        return RegControlsF._pt_ratio_read(self)

    @pt_ratio.setter
    def pt_ratio(self, arg: float):
        RegControlsF._pt_ratio_write(self, arg)

    @property
    def forward_r(self) -> float:
        return RegControlsF._forward_r_read(self)

    @forward_r.setter
    def forward_r(self, arg: float):
        RegControlsF._forward_r_write(self, arg)

    @property
    def forward_x(self) -> float:
        return RegControlsF._forward_x_read(self)

    @forward_x.setter
    def forward_x(self, arg: float):
        RegControlsF._forward_x_write(self, arg)

    @property
    def reverse_r(self) -> float:
        return RegControlsF._reverse_r_read(self)

    @reverse_r.setter
    def reverse_r(self, arg: float):
        RegControlsF._reverse_r_write(self, arg)

    @property
    def reverse_x(self) -> float:
        return RegControlsF._reverse_x_read(self)

    @reverse_x.setter
    def reverse_x(self, arg: float):
        RegControlsF._reverse_x_write(self, arg)

    @property
    def delay(self) -> float:
        return RegControlsF._delay_read(self)

    @delay.setter
    def delay(self, arg: float):
        RegControlsF._delay_write(self, arg)

    @property
    def tap_delay(self) -> float:
        return RegControlsF._tap_delay_read(self)

    @tap_delay.setter
    def tap_delay(self, arg: float):
        RegControlsF._tap_delay_write(self, arg)

    @property
    def voltage_limit(self) -> float:
        return RegControlsF._voltage_limit_read(self)

    @voltage_limit.setter
    def voltage_limit(self, arg: float):
        RegControlsF._voltage_limit_write(self, arg)

    @property
    def forward_band(self) -> float:
        return RegControlsF._forward_band_read(self)

    @forward_band.setter
    def forward_band(self, arg: float):
        RegControlsF._forward_band_write(self, arg)

    @property
    def forward_vreg(self) -> float:
        return RegControlsF._forward_vreg_read(self)

    @forward_vreg.setter
    def forward_vreg(self, arg: float):
        RegControlsF._forward_vreg_write(self, arg)

    @property
    def reverse_band(self) -> float:
        return RegControlsF._reverse_band_read(self)

    @reverse_band.setter
    def reverse_band(self, arg: float):
        RegControlsF._reverse_band_write(self, arg)

    @property
    def reverse_vreg(self) -> float:
        return RegControlsF._reverse_vreg_read(self)

    @reverse_vreg.setter
    def reverse_vreg(self, arg: float):
        RegControlsF._reverse_vreg_write(self, arg)

    def first(self) -> int:
        return RegControlsI._first(self)

    def next(self) -> int:
        return RegControlsI._next(self)

    @property
    def tap_winding(self) -> int:
        return RegControlsI._tap_winding_read(self)

    @tap_winding.setter
    def tap_winding(self, arg: int):
        RegControlsI._tap_winding_write(self, arg)

    @property
    def winding(self) -> int:
        return RegControlsI._winding_read(self)

    @winding.setter
    def winding(self, arg: int):
        RegControlsI._winding_write(self, arg)

    @property
    def is_reversible(self) -> int:
        return RegControlsI._is_reversible_read(self)

    @is_reversible.setter
    def is_reversible(self, arg: int):
        RegControlsI._is_reversible_write(self, arg)

    @property
    def is_inverse_time(self) -> int:
        return RegControlsI._is_inverse_time_read(self)

    @is_inverse_time.setter
    def is_inverse_time(self, arg: int):
        RegControlsI._is_inverse_time_write(self, arg)

    @property
    def max_tap_change(self) -> int:
        return RegControlsI._max_tap_change_read(self)

    @max_tap_change.setter
    def max_tap_change(self, arg: int):
        RegControlsI._max_tap_change_write(self, arg)

    @property
    def count(self) -> int:
        return RegControlsI._count(self)

    @property
    def tap_number(self) -> int:
        return RegControlsI._tap_number_read(self)

    @tap_number.setter
    def tap_number(self, arg: int):
        RegControlsI._tap_number_write(self, arg)

    @property
    def name(self) -> str:
        return RegControlsS._name_read(self)

    @name.setter
    def name(self, arg: str):
        RegControlsS._name_write(self, arg)

    @property
    def monitored_bus(self) -> str:
        return RegControlsS._monitored_bus_read(self)

    @monitored_bus.setter
    def monitored_bus(self, arg: str):
        RegControlsS._monitored_bus_write(self, arg)

    @property
    def transformer(self) -> str:
        return RegControlsS._transformer_read(self)

    @transformer.setter
    def transformer(self, arg: str):
        RegControlsS._transformer_write(self, arg)

    @property
    def names(self) -> List[str]:
        return RegControlsV._names(self)
