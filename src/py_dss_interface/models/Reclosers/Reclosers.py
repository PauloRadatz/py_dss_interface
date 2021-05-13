# -*- coding: iso-8859-15 -*-

from py_dss_interface.models.Reclosers.ReclosersF import ReclosersF
from py_dss_interface.models.Reclosers.ReclosersI import ReclosersI
from py_dss_interface.models.Reclosers.ReclosersS import ReclosersS
from py_dss_interface.models.Reclosers.ReclosersV import ReclosersV


class Reclosers(ReclosersI, ReclosersV, ReclosersS, ReclosersF):
    """
    This interface implements the Reclosers (IReclosers) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ReclosersI, ReclosersV, ReclosersS, ReclosersF.
    """
    pass
