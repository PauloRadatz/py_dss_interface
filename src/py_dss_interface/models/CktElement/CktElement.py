# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.CktElement.CktElementF import CktElementF
from py_dss_interface.models.CktElement.CktElementI import CktElementI
from py_dss_interface.models.CktElement.CktElementS import CktElementS
from py_dss_interface.models.CktElement.CktElementV import CktElementV


class CktElement(CktElementI, CktElementS, CktElementF, CktElementV):
    """
    This interface implements the CktElement interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: CktElementI, CktElementS, CktElementF, CktElementV
    """
    pass
