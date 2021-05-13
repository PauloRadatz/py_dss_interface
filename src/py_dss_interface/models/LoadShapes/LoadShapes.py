# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.LoadShapes.LoadShapesF import LoadShapesF
from py_dss_interface.models.LoadShapes.LoadShapesI import LoadShapesI
from py_dss_interface.models.LoadShapes.LoadShapesS import LoadShapesS
from py_dss_interface.models.LoadShapes.LoadShapesV import LoadShapesV


class LoadShapes(LoadShapesF, LoadShapesI, LoadShapesS, LoadShapesV):
    """
    This interface implements the LoadShape (ILoadShape) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """
    pass
