# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.LineCodes.LineCodesF import LineCodesF
from py_dss_interface.models.LineCodes.LineCodesI import LineCodesI
from py_dss_interface.models.LineCodes.LineCodesS import LineCodesS
from py_dss_interface.models.LineCodes.LineCodesV import LineCodesV


class LineCodes(LineCodesF, LineCodesS, LineCodesI, LineCodesV):
    """
    This interface implements the Lines (ILineCodes) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface:
    """
    pass
