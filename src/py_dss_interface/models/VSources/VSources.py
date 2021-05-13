# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.VSources.VSourcesF import VSourcesF
from py_dss_interface.models.VSources.VSourcesI import VSourcesI
from py_dss_interface.models.VSources.VSourcesS import VSourcesS
from py_dss_interface.models.VSources.VSourcesV import VSourcesV


class VSources(VSourcesS, VSourcesV, VSourcesI, VSourcesF):
    """
    This interface implements the Vsources (IVSources) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: VSourcesS, VSourcesV, VSourcesI, VSourcesF.
    """
    pass
