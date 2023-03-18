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
        """Gets the CT primary ampere rating (secondary is 0.2 amperes).
        Sets the CT primary ampere rating (secondary is 0.2 amperes)."""
        return RegControlsF._ct_primary_read(self)

    @ct_primary.setter
    def ct_primary(self, arg: float):
        RegControlsF._ct_primary_write(self, arg)

    @property
    def pt_ratio(self) -> float:
        """Gets the PT ratio for voltage control settings.
        Sets the PT ratio for voltage control settings."""
        return RegControlsF._pt_ratio_read(self)

    @pt_ratio.setter
    def pt_ratio(self, arg: float):
        RegControlsF._pt_ratio_write(self, arg)

    @property
    def forward_r(self) -> float:
        """Gets the LDC R settings in Volts.
        Sets the LDC R settings in Volts."""
        return RegControlsF._forward_r_read(self)

    @forward_r.setter
    def forward_r(self, arg: float):
        RegControlsF._forward_r_write(self, arg)

    @property
    def forward_x(self) -> float:
        """Gets the LDC X settings in Volts.
        Sets sets the LDC X settings in Volts."""
        return RegControlsF._forward_x_read(self)

    @forward_x.setter
    def forward_x(self, arg: float):
        RegControlsF._forward_x_write(self, arg)

    @property
    def reverse_r(self) -> float:
        """Gets the reverse LDC R settings in Volts.
        Sets the reverse LDC R settings in Volts."""
        return RegControlsF._reverse_r_read(self)

    @reverse_r.setter
    def reverse_r(self, arg: float):
        RegControlsF._reverse_r_write(self, arg)

    @property
    def reverse_x(self) -> float:
        """Gets the reverse LDC X settings in Volts.
        Sets the reverse LDC X settings in Volts."""
        return RegControlsF._reverse_x_read(self)

    @reverse_x.setter
    def reverse_x(self, arg: float):
        RegControlsF._reverse_x_write(self, arg)

    @property
    def delay(self) -> float:
        """Gets the time delay [s] after arming before the first tap change.
                Control may reset before actually changing taps.
        Sets the time delay [s] after arming before the first tap change. Control may reset before actually
                changing taps. """
        return RegControlsF._delay_read(self)

    @delay.setter
    def delay(self, arg: float):
        RegControlsF._delay_write(self, arg)

    @property
    def tap_delay(self) -> float:
        """Gets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
                taps.
        Sets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
                taps."""
        return RegControlsF._tap_delay_read(self)

    @tap_delay.setter
    def tap_delay(self, arg: float):
        RegControlsF._tap_delay_write(self, arg)

    @property
    def voltage_limit(self) -> float:
        return RegControlsF._voltage_limit_read(self)

    @voltage_limit.setter
    def voltage_limit(self, arg: float):
        """Gets the first house voltage limit on PT secondary base. Setting to 0 disables this function.
        Sets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        RegControlsF._voltage_limit_write(self, arg)

    @property
    def forward_band(self) -> float:
        """Gets the regulation bandwidth in forward direction, centered on Vreg.
        Sets the regulation bandwidth in forward direction, centered on Vreg."""
        return RegControlsF._forward_band_read(self)

    @forward_band.setter
    def forward_band(self, arg: float):
        RegControlsF._forward_band_write(self, arg)

    @property
    def forward_vreg(self) -> float:
        """Gets the target voltage in the forward direction, on PT secondary base.
        Sets the target voltage in the forward direction, on PT secondary base."""
        return RegControlsF._forward_vreg_read(self)

    @forward_vreg.setter
    def forward_vreg(self, arg: float):
        RegControlsF._forward_vreg_write(self, arg)

    @property
    def reverse_band(self) -> float:
        """Gets the bandwidth in reverse direction, centered on reverse Vreg.
        Sets the bandwidth in reverse direction, centered on reverse Vreg."""
        return RegControlsF._reverse_band_read(self)

    @reverse_band.setter
    def reverse_band(self, arg: float):
        RegControlsF._reverse_band_write(self, arg)

    @property
    def reverse_vreg(self) -> float:
        """Gets the target voltage in the reverse direction, on PT secondary base.
        Sets the target voltage in the reverse direction, on PT secondary base."""
        return RegControlsF._reverse_vreg_read(self)

    @reverse_vreg.setter
    def reverse_vreg(self, arg: float):
        RegControlsF._reverse_vreg_write(self, arg)

    def first(self) -> int:
        """Sets the first RegControl active. Returns 0 if no more."""
        return RegControlsI._first(self)

    def next(self) -> int:
        """Sets the next RegControl active. Returns 0 if no more"""
        return RegControlsI._next(self)

    @property
    def tap_winding(self) -> int:
        """Gets the tapped winding number.
        Sets the tapped winding number."""
        return RegControlsI._tap_winding_read(self)

    @tap_winding.setter
    def tap_winding(self, arg: int):
        RegControlsI._tap_winding_write(self, arg)

    @property
    def winding(self) -> int:
        """Gets the winding number for PT and CT connections.
        Sets the winding number for PT and CT connections."""
        return RegControlsI._winding_read(self)

    @winding.setter
    def winding(self, arg: int):
        RegControlsI._winding_write(self, arg)

    @property
    def is_reversible(self) -> int:
        """Gets the setting in the reverse direction, usually not applicable to substation transformers.
        Sets the different settings for the reverse direction (see Manual for details),
        usually not applicable to substation transformers."""
        return RegControlsI._is_reversible_read(self)

    @is_reversible.setter
    def is_reversible(self, arg: int):
        RegControlsI._is_reversible_write(self, arg)

    @property
    def is_inverse_time(self) -> int:
        """Gets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
                 outside the regulator band.
        Sets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        return RegControlsI._is_inverse_time_read(self)

    @is_inverse_time.setter
    def is_inverse_time(self, arg: int):
        RegControlsI._is_inverse_time_write(self, arg)

    @property
    def max_tap_change(self) -> int:
        """Gets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
                 faster solution.
        Sets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        return RegControlsI._max_tap_change_read(self)

    @max_tap_change.setter
    def max_tap_change(self, arg: int):
        RegControlsI._max_tap_change_write(self, arg)

    @property
    def count(self) -> int:
        """Gets the number of RegControl objects in Active Circuit."""
        return RegControlsI._count(self)

    @property
    def tap_number(self) -> int:
        """Gets the actual tap number of the active RegControl.
        Sets the actual tap number of the active RegControl."""
        return RegControlsI._tap_number_read(self)

    @tap_number.setter
    def tap_number(self, arg: int):
        RegControlsI._tap_number_write(self, arg)

    @property
    def name(self) -> str:
        """Gets the active RegControl name.
        Sets the active RegControl name."""
        return RegControlsS._name_read(self)

    @name.setter
    def name(self, arg: str):
        RegControlsS._name_write(self, arg)

    @property
    def monitored_bus(self) -> str:
        """Gets the name of the remote regulated bus, in lieu of LDC settings.
        Sets the name of the remote regulated bus, in lieu of LDC settings."""
        return RegControlsS._monitored_bus_read(self)

    @monitored_bus.setter
    def monitored_bus(self, arg: str):
        RegControlsS._monitored_bus_write(self, arg)

    @property
    def transformer(self) -> str:
        """Gets the name of the transformer this regulator controls.
        Sets the name of the transformer this regulator controls."""
        return RegControlsS._transformer_read(self)

    @transformer.setter
    def transformer(self, arg: str):
        RegControlsS._transformer_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings containing all RegControl names."""
        return RegControlsV._names(self)
