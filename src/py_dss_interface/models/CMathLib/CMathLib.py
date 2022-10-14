# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.CMathLib.CMathLibF import CMathLibF
from py_dss_interface.models.CMathLib.CMathLibV import CMathLibV


class CMathLib(CMathLibF, CMathLibV):
    """
    This interface implements the CmathLib (ICmathLib) interface of OpenDSS by declaring 2 procedures for accessing
    the different properties included in this interface: CMathLibF, CMathLibV.
    """
    pass
