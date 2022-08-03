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
        return SensorsF.pct_error(self)

    @pct_error.setter
    def pct_error(self, argument):
        SensorsF.pct_error_write(self, argument)

    @property
    def weight(self) -> float:
        return SensorsF.weight(self)

    @weight.setter
    def weight(self, argument):
        SensorsF.weight_write(self, argument)

    @property
    def kv_base(self) -> float:
        return SensorsF.kv_base(self)

    @kv_base.setter
    def kv_base(self, argument):
        SensorsF.kv_base_write(self, argument)

    @property
    def count(self) -> int:
        return SensorsI.count(self)

    @property
    def first(self) -> int:
        return SensorsI.first(self)

    @property
    def next(self) -> int:
        return SensorsI.next(self)

    @property
    def is_delta(self) -> int:
        return SensorsI.is_delta(self)

    @is_delta.setter
    def is_delta(self, argument):
        SensorsI.is_delta_write(self, argument)

    @property
    def reverse_delta(self) -> int:
        return SensorsI.reverse_delta(self)

    @reverse_delta.setter
    def reverse_delta(self, argument):
        SensorsI.reverse_delta_write(self, argument)

    @property
    def metered_terminal(self) -> int:
        return SensorsI.metered_terminal(self)

    @metered_terminal.setter
    def metered_terminal(self, argument):
        SensorsI.metered_terminal_write(self, argument)

    @property
    def reset(self) -> int:
        return SensorsI.reset(self)

    @property
    def reset_all(self) -> int:
        return SensorsI.reset_all(self)

    @property
    def name(self):
        return SensorsS.name(self)

    @name.setter
    def name(self, argument):
        SensorsS.name_write(self, argument)

    @property
    def metered_element(self) -> str:
        return SensorsS.metered_element(self)

    @metered_element.setter
    def metered_element(self, argument):
        SensorsS.metered_element_write(self, argument)

    @property
    def all_names(self):
        return SensorsV.all_names(self)

    @property
    def currents(self):
        return SensorsV.currents(self)

    @currents.setter
    def currents(self, argument):
        SensorsV.currents_write(self, argument)

    @property
    def kvars(self):
        return SensorsV.kvars(self)

    @kvars.setter
    def kvars(self, argument):
        SensorsV.kvars_write(self, argument)

    @property
    def kws(self):
        return SensorsV.kws(self)

    @kws.setter
    def kws(self, argument):
        SensorsV.kws_write(self, argument)
