# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Fuses.FusesF import FusesF
from py_dss_interface.models.Fuses.FusesI import FusesI
from py_dss_interface.models.Fuses.FusesS import FusesS
from py_dss_interface.models.Fuses.FusesV import FusesV


class Fuses(FusesI, FusesS, FusesF, FusesV):
    """" This interface implements the Fuses (IFuses) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: FusesI, FusesS, FusesF, FusesV """
    pass
