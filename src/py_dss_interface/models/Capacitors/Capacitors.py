# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.PD_Elements.Capacitors.CapacitorsI import CapacitorsI
from py_dss_interface.models.PD_Elements.Capacitors.CapacitorsS import CapacitorsS
from py_dss_interface.models.PD_Elements.Capacitors.CapacitorsV import CapacitorsV
from py_dss_interface.models.PD_Elements.Capacitors.CapacitorsF import CapacitorsF


class Capacitors(CapacitorsF, CapacitorsI, CapacitorsS, CapacitorsV):
    """
    This interface implements the Capacitors (ICapacitors) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: CapacitorsF, CapacitorsI, CapacitorsS, CapacitorsV.
    """
    pass
