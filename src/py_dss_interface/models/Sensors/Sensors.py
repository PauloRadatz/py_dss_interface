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
    pass
