# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Settings.SettingsF import SettingsF
from py_dss_interface.models.Settings.SettingsI import SettingsI
from py_dss_interface.models.Settings.SettingsS import SettingsS
from py_dss_interface.models.Settings.SettingsV import SettingsV


class Settings(SettingsS, SettingsF, SettingsI, SettingsV):
    """
    This interface implements the Settings (ISettings) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: SettingsS, SettingsF, SettingsI, SettingsV.
    """
    pass
