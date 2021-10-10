# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Lines.LinesF import LinesF
from py_dss_interface.models.Lines.LinesI import LinesI
from py_dss_interface.models.Lines.LinesS import LinesS
from py_dss_interface.models.Lines.LinesV import LinesV


class Lines(LinesV, LinesS, LinesI, LinesF):
    """
    This interface implements the Lines (ILines) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: LinesV, LinesS, LinesI, LinesF.
    """
    pass
