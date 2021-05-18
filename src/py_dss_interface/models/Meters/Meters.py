# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Meters.MetersF import MetersF
from py_dss_interface.models.Meters.MetersI import MetersI
from py_dss_interface.models.Meters.MetersS import MetersS
from py_dss_interface.models.Meters.MetersV import MetersV


class Meters(MetersV, MetersS, MetersF, MetersI):
    """
    This interface implements the Meters (IMeters) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: MetersV, MetersS, MetersF, MetersI.
    """
    pass
