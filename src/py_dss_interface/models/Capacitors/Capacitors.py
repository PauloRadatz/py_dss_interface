# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Capacitors.CapacitorsI import CapacitorsI
from py_dss_interface.models.Capacitors.CapacitorsS import CapacitorsS
from py_dss_interface.models.Capacitors.CapacitorsV import CapacitorsV
from py_dss_interface.models.Capacitors.CapacitorsF import CapacitorsF


class Capacitors(CapacitorsF, CapacitorsI, CapacitorsS, CapacitorsV):
    pass
