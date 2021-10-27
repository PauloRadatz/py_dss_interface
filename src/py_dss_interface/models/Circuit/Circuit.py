# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Circuit.CircuitF import CircuitF
from py_dss_interface.models.Circuit.CircuitI import CircuitI
from py_dss_interface.models.Circuit.CircuitS import CircuitS
from py_dss_interface.models.Circuit.CircuitV import CircuitV


class Circuit(CircuitI, CircuitS, CircuitF, CircuitV):
    """
    This interface implements the Circuit (ICIrcuit) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: CircuitI, CircuitS, CircuitF, CircuitV
    """
    pass
