# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.ActiveClass.ActiveClassS import ActiveClassS
from py_dss_interface.models.ActiveClass.ActiveClassI import ActiveClassI
from py_dss_interface.models.ActiveClass.ActiveClassV import ActiveClassV


class ActiveClass(ActiveClassS, ActiveClassI, ActiveClassV):
    pass
