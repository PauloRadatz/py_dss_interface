# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.DSSElement.DSSElementI import DSSElementI
from py_dss_interface.models.DSSElement.DSSElementS import DSSElementS
from py_dss_interface.models.DSSElement.DSSElementV import DSSElementV


class DSSElement(DSSElementI, DSSElementS, DSSElementV):
    """
    This interface implements the DSSElement (IDSSElement) interface of OpenDSS by declaring 3 procedures for
    accessing the different properties included in this interface: DSSElementI, DSSElementS, DSSElementV
    """
    pass
