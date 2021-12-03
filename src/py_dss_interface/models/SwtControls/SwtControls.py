# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.SwtControls.SwtControlsF import SwtControlsF
from py_dss_interface.models.SwtControls.SwtControlsI import SwtControlsI
from py_dss_interface.models.SwtControls.SwtControlsS import SwtControlsS
from py_dss_interface.models.SwtControls.SwtControlsV import SwtControlsV


class SwtControls(SwtControlsS, SwtControlsV, SwtControlsI, SwtControlsF):
    """
    This interface implements the SwtControls (ISwtControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: SwtControlsS, SwtControlsV, SwtControlsI,
    SwtControlsF.
    """
    pass
