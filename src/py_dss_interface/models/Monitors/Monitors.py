# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Monitors.MonitorsI import MonitorsI
from py_dss_interface.models.Monitors.MonitorsS import MonitorsS
from py_dss_interface.models.Monitors.MonitorsV import MonitorsV


class Monitors(MonitorsI, MonitorsV, MonitorsS):
    """
    This interface implements the Monitors (IMonitors) interface of OpenDSS by declaring 3 procedures for accessing
    the different properties included in this interface: MonitorsI, MonitorsV, MonitorsS.
    """
    pass
