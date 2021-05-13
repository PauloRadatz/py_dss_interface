# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Loads.LoadsF import LoadsF
from py_dss_interface.models.Loads.LoadsI import LoadsI
from py_dss_interface.models.Loads.LoadsS import LoadsS
from py_dss_interface.models.Loads.LoadsV import LoadsV


class Loads(LoadsF, LoadsI, LoadsS, LoadsV):
    """
    This interface implements the Loads (ILoads) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: LoadsF, LoadsI, LoadsS, LoadsV.
    """
    pass
