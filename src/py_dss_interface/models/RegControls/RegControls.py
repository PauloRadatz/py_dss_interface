# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.RegControls.RegControlsF import RegControlsF
from py_dss_interface.models.RegControls.RegControlsI import RegControlsI
from py_dss_interface.models.RegControls.RegControlsS import RegControlsS
from py_dss_interface.models.RegControls.RegControlsV import RegControlsV


class RegControls(RegControlsI, RegControlsF, RegControlsV, RegControlsS):
    """
    This interface implements the RegControls (IRegControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: RegControlsI, RegControlsF, RegControlsV,
    RegControlsS.
    """
    pass
