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
        """Gets the type of automatic controller (see manual for details).

                CURRENTCONTROL: Result := 0;

                VOLTAGECONTROL: Result := 1;

                VARCONTROL: Result := 2;

                TIMECONTROL: Result := 3;

                PFCONTROL: Result := 4;

                USERCONTROL: Result := 4;

        Sets the type of automatic controller (see manual for details).

        0: elem.CapControlType := CURRENTCONTROL;

        1: elem.CapControlType := VOLTAGECONTROL;

        2: elem.CapControlType := KVARCONTROL;

        3: elem.CapControlType := TIMECONTROL;

        4: elem.CapControlType := PFCONTROL;
        """
        return CapControlsI._mode(self)

    @mode.setter
    def mode(self, argument: int):
        CapControlsI._mode_write(self, argument)

    @property
    def monitored_term(self) -> int:
        """Gets the terminal number on the element that PT and CT are connected to.

        Sets the terminal number on the element that PT and CT are connected to. There is not a explicit return
        type in the oficial documentation, because of this we choose not put a explicit return too. """
        return CapControlsI._monitored_term(self)

    @monitored_term.setter
    def monitored_term(self, values):
        dss, argument = values
        CapControlsI._monitored_term_write(self, dss, argument)

    @property
    def use_volt_override(self) -> int:
        """Gets if Vmin and Vmax are enabled to override the control Mode. There is not a explicit return type in the
                oficial documentation, because of this we choose not put a explicit return too.

        Sets if enables Vmin and Vmax to override the control Mode. There is not a explicit return type in the
        oficial documentation, because of this we choose not put a explicit return too. """

        return CapControlsI._use_volt_override(self)

    @use_volt_override.setter
    def use_volt_override(self, values):
        dss, argument = values
        CapControlsI._use_volt_override_write(self, dss, argument)

    @property
    def count(self) -> int:
        """Gets the number of CapControls in Active Circuit."""
        return CapControlsI._count(self)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of string with all CapControl names."""
        return CapControlsV._names(self)

    @property
    def ct_ratio(self) -> float:
        """Gets the transducer ratio current to control current.

        Sets the transducer ratio current to control current."""
        return CapControlsF._ct_ratio(self)

    @ct_ratio.setter
    def ct_ratio(self, argument: float):
        CapControlsF._ct_ratio_write(self, argument)

    @property
    def pt_ratio(self) -> float:
        return CapControlsF._pt_ratio(self)

    @pt_ratio.setter
    def pt_ratio(self, argument: float):
        """Gets the transducer ratio from primary feeder to control voltage.

        Sets the transducer ratio from primary feeder to control voltage."""
        CapControlsF._pt_ratio_write(self, argument)

    @property
    def on_setting(self) -> float:
        """Gets the threshold to arm or switch on a step. See Mode for Units.

        Sets the threshold to arm or switch on a step. See Mode for Units."""
        return CapControlsF._on_setting(self)

    @on_setting.setter
    def on_setting(self, argument: float):
        CapControlsF._on_setting_write(self, argument)

    @property
    def off_setting(self) -> float:
        return CapControlsF._off_setting(self)

    @off_setting.setter
    def off_setting(self, argument: float):
        """Gets the threshold to switch off a step. See Mode for Units.

        Sets the threshold to switch off a step. See Mode for Units."""
        CapControlsF._off_setting_write(self, argument)

    @property
    def vmax(self) -> float:
        """Gets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level.

        Sets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        return CapControlsF._vmax(self)

    @vmax.setter
    def vmax(self, argument: float):
        CapControlsF._vmax_write(self, argument)

    @property
    def vmin(self) -> float:
        """Gets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level.

        Sets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        return CapControlsF._vmin(self)

    @vmin.setter
    def vmin(self, argument: float):
        CapControlsF._vmin_write(self, argument)

    @property
    def delay(self) -> float:
        """Gets the time delay [s] to switch on after arming. Control may reset before actually switching.

        Sets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        return CapControlsF._delay(self)

    @delay.setter
    def delay(self, argument: float):
        CapControlsF._delay_write(self, argument)

    @property
    def delay_off(self) -> float:
        """Gets the time delay [s] before switching off a step. Control may reset before actually switching.

        Sets the time delay [s] before switching off a step. Control may reset before actually switching."""
        return CapControlsF._delay_off(self)

    @delay_off.setter
    def delay_off(self, argument: float):
        CapControlsF._delay_off_write(self, argument)

    @property
    def dead_time(self) -> float:
        """Gets the time delay [s] after switching off a step. Control may reset before actually switching.

        Sets the time delay [s] after switching off a step. Control may reset before actually switching.."""
        return CapControlsF._dead_time(self)

    @dead_time.setter
    def dead_time(self, argument: float):
        CapControlsF._dead_time_write(self, argument)

    @property
    def name(self) -> str:
        """Gets the name of the active CapControl.

        Sets a CapControl active by name."""
        return CapControlsS._name(self)

    @name.setter
    def name(self, argument: str):
        CapControlsS._name_write(self, argument)

    @property
    def controlled_capacitor(self) -> str:
        """Gets the name of the capacitor that is controlled.

        Sets the name of the capacitor that is controlled."""
        return CapControlsS._controlled_capacitor(self)

    @controlled_capacitor.setter
    def controlled_capacitor(self, argument: str):
        CapControlsS._controlled_capacitor_write(self, argument)

    @property
    def monitored_object(self) -> str:
        """Gets the full name of the element that PT and CT are connected to.

        Sets the full name of the element that PT and CT are connected to."""
        return CapControlsS._monitored_object(self)

    @monitored_object.setter
    def monitored_object(self, argument: str):
        CapControlsS._monitored_object_write(self, argument)

    def first(self) -> int:
        """Sets the first CapControl active. Returns 0 if no more."""
        return CapControlsI._first(self)

    def next(self) -> int:
        """Sets the next CapControl active. Returns 0 if no more."""
        return CapControlsI._next(self)
