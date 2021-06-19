# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.PVSystems.PVSystemsF import PVSystemsF
from py_dss_interface.models.PVSystems.PVSystemsI import PVSystemsI
from py_dss_interface.models.PVSystems.PVSystemsS import PVSystemsS
from py_dss_interface.models.PVSystems.PVSystemsV import PVSystemsV


class PVSystems(PVSystemsV, PVSystemsS, PVSystemsI, PVSystemsF):
    """
    This interface implements the PVSystems (IPVSystems) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: PVSystemsV, PVSystemsS, PVSystemsI, PVSystemsF.
    """
    pass
