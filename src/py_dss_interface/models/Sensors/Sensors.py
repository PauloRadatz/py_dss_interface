# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Sensors.SensorsF import SensorsF
from py_dss_interface.models.Sensors.SensorsI import SensorsI
from py_dss_interface.models.Sensors.SensorsS import SensorsS
from py_dss_interface.models.Sensors.SensorsV import SensorsV
from typing import List


class Sensors(SensorsV, SensorsS, SensorsI, SensorsF):
    """
    This interface implements the Sensors (ISensors) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: SensorsV, SensorsS, SensorsI, SensorsF.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def pct_error(self) -> float:
        """Gets the assumed percent error in the Sensor measurement. Default is 1.
        Sets the assumed percent error in the Sensor measurement. Default is 1."""
        return SensorsF._pct_error_read(self)

    @pct_error.setter
    def pct_error(self, arg: float):
        SensorsF._pct_error_write(self, arg)

    @property
    def weight(self) -> float:
        """Gets the weighting factor for this sensor measurement with respect to the other sensors. Default is 1.
        Sets the weighting factor for this sensor measurement with respect to the other sensors. Default is 1."""
        return SensorsF._weight_read(self)

    @weight.setter
    def weight(self, arg: float):
        SensorsF._weight_write(self, arg)

    @property
    def kv_base(self) -> float:
        """Gets the voltage base for the sensor measurements. LL for 2 and 3 - phase sensors, LN for 1-phase sensors.
        Sets the voltage base for the sensor measurements. LL for 2 and 3 - phase sensors, LN for 1-phase sensors."""
        return SensorsF._kv_base_read(self)

    @kv_base.setter
    def kv_base(self, arg: float):
        SensorsF._kv_base_write(self, arg)

    @property
    def _count(self) -> int:
        """Gets number of sensors in active circuit."""
        return SensorsI._count(self)

    def first(self) -> int:
        """Sets the first sensor active. Returns 0 if none."""
        return SensorsI._first(self)

    def next(self) -> int:
        """Sets the next sensor active. Returns 0 if none."""
        return SensorsI._next(self)

    @property
    def is_delta(self) -> int:
        """Returns 1 if the sensor is connected in delta; otherwise, returns 0.
        Allows to set 1 if the sensor is connected in delta; otherwise, set 0 (argument)."""
        return SensorsI._is_delta_read(self)

    @is_delta.setter
    def is_delta(self, arg: int):
        SensorsI._is_delta_write(self, arg)

    @property
    def reverse_delta(self) -> int:
        """Returns 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0.
        Allows to set 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        return SensorsI._reverse_delta_read(self)

    @reverse_delta.setter
    def reverse_delta(self, arg: int):
        SensorsI._reverse_delta_write(self, arg)

    @property
    def metered_terminal(self) -> int:
        """Gets the number of the measured terminal in the measured element.
        Sets the number of the measured terminal in the measured element."""
        return SensorsI._metered_terminal_read(self)

    @metered_terminal.setter
    def metered_terminal(self, arg: int):
        SensorsI._metered_terminal_write(self, arg)

    def reset(self) -> int:
        """Clears the active sensor."""
        return SensorsI._reset(self)

    def reset_all(self) -> int:
        """Clears all sensors in the active circuit."""
        return SensorsI._reset_all(self)

    @property
    def name(self) -> str:
        """Gets the name of the active sensor object.
        Sets the name of the active sensor object."""
        return SensorsS._name_read(self)

    @name.setter
    def name(self, arg: str):
        SensorsS._name_write(self, arg)

    @property
    def metered_element(self) -> str:
        """Gets the full name of the measured element.
        Sets the full name of the measured element."""
        return SensorsS._metered_element_read(self)

    @metered_element.setter
    def metered_element(self, arg: str):
        SensorsS._metered_element_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Returns a variant array of sensor names."""
        return SensorsV._names(self)

    @property
    def currents(self) -> List[float]:
        """Gets an array of doubles for the line current measurements; don't use with KWS and KVARS.
        Sets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        return SensorsV._currents_read(self)

    @currents.setter
    def currents(self, arg: List[float]):
        SensorsV._currents_write(self, arg)

    @property
    def kvars(self) -> List[float]:
        """Gets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS.
        Sets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        return SensorsV._kvars_read(self)

    @kvars.setter
    def kvars(self, arg: List[float]):
        SensorsV._kvars_write(self, arg)

    @property
    def kws(self) -> List[float]:
        """Gets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS.
        Sets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        return SensorsV._kws_read(self)

    @kws.setter
    def kws(self, arg: List[float]):
        SensorsV._kws_write(self, arg)
