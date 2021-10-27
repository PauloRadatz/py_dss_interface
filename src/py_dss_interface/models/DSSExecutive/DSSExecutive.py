# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.DSSExecutive.DSSExecutiveI import DSSExecutiveI
from py_dss_interface.models.DSSExecutive.DSSExecutiveS import DSSExecutiveS


class DSSExecutive(DSSExecutiveS, DSSExecutiveI):
    """
    This interface implements the DSS_Executive (IDSS_Executive) interface of OpenDSS by declaring 2 procedures for
    accessing the different properties included in this interface: DSSExecutiveS, DSSExecutiveI
    """
    pass
