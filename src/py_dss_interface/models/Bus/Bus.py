# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Bus.BusF import BusF
from py_dss_interface.models.Bus.BusI import BusI
from py_dss_interface.models.Bus.BusS import BusS
from py_dss_interface.models.Bus.BusV import BusV


class Bus(BusS, BusI, BusV, BusF):
    """
    This interface implements the Bus (IBus) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: BusS, BusI, BusV, BusF.
    """
    pass
