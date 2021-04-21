# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Bus.BusS import BusS
from py_dss_interface.models.Bus.BusI import BusI
from py_dss_interface.models.Bus.BusV import BusV
from py_dss_interface.models.Bus.BusF import BusF


class Bus(BusS, BusI, BusV, BusF):
    pass
