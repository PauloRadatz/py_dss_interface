# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Sensors.SensorsF import SensorsF
from py_dss_interface.models.Sensors.SensorsI import SensorsI
from py_dss_interface.models.Sensors.SensorsS import SensorsS
from py_dss_interface.models.Sensors.SensorsV import SensorsV


class Sensors(SensorsV, SensorsS, SensorsI, SensorsF):
    """
    This interface implements the Sensors (ISensors) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: SensorsV, SensorsS, SensorsI, SensorsF.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def pct_error(self) -> float:
        return SensorsF._pct_error(self)

    @pct_error.setter
    def pct_error(self, argument):
        SensorsF._pct_error_write(self, argument)

    @property
    def _weight(self) -> float:
        return SensorsF._weight(self)

    @weight.setter
    def weight(self, argument):
        SensorsF._weight_write(self, argument)

    @property
    def kv_base(self) -> float:
        return SensorsF._kv_base(self)

    @kv_base.setter
    def kv_base(self, argument):
        SensorsF._kv_base_write(self, argument)

    @property
    def _count(self) -> int:
        return SensorsI._count(self)

    @property
    def first(self) -> int:
        return SensorsI._first(self)

    @property
    def next(self) -> int:
        return SensorsI._next(self)

    @property
    def is_delta(self) -> int:
        return SensorsI._is_delta(self)

    @is_delta.setter
    def is_delta(self, argument):
        SensorsI._is_delta_write(self, argument)

    @property
    def reverse_delta(self) -> int:
        return SensorsI._reverse_delta(self)

    @reverse_delta.setter
    def reverse_delta(self, argument):
        SensorsI._reverse_delta_write(self, argument)

    @property
    def metered_terminal(self) -> int:
        return SensorsI._metered_terminal(self)

    @metered_terminal.setter
    def metered_terminal(self, argument):
        SensorsI._metered_terminal_write(self, argument)

    @property
    def reset(self) -> int:
        return SensorsI._reset(self)

    @property
    def reset_all(self) -> int:
        return SensorsI._reset_all(self)

    @property
    def name(self):
        return SensorsS._name(self)

    @name.setter
    def name(self, argument):
        SensorsS._name_write(self, argument)

    @property
    def metered_element(self) -> str:
        return SensorsS._metered_element(self)

    @metered_element.setter
    def metered_element(self, argument):
        SensorsS._metered_element_write(self, argument)

    @property
    def all_names(self):
        return SensorsV._all_names(self)

    @property
    def currents(self):
        return SensorsV._currents(self)

    @currents.setter
    def currents(self, argument):
        SensorsV._currents_write(self, argument)

    @property
    def kvars(self):
        return SensorsV._kvars(self)

    @kvars.setter
    def kvars(self, argument):
        SensorsV._kvars_write(self, argument)

    @property
    def kws(self):
        return SensorsV._kws(self)

    @kws.setter
    def kws(self, argument):
        SensorsV._kws_write(self, argument)
