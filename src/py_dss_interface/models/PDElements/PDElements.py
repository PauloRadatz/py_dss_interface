# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.PDElements.PDElementsF import PDElementsF
from py_dss_interface.models.PDElements.PDElementsI import PDElementsI
from py_dss_interface.models.PDElements.PDElementsS import PDElementsS


class PDElements(PDElementsF, PDElementsI, PDElementsS):
    """
    This interface implements the PDElements (IPDElements) interface of OpenDSS by declaring 3 procedures for
    accessing the different properties included in this interface: PDElementsF, PDElementsI, PDElementsS.
    """
    pass
