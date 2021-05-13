# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.SWTControls.SWTControlsF import SWTControlsF
from py_dss_interface.models.SWTControls.SWTControlsI import SWTControlsI
from py_dss_interface.models.SWTControls.SWTControlsS import SWTControlsS
from py_dss_interface.models.SWTControls.SWTControlsV import SWTControlsV


class SWTControls(SWTControlsS, SWTControlsV, SWTControlsI, SWTControlsF):
    """
    This interface implements the SwtControls (ISwtControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: SWTControlsS, SWTControlsV, SWTControlsI,
    SWTControlsF.
    """
    pass
