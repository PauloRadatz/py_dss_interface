# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.ActiveClass.ActiveClassI import ActiveClassI
from py_dss_interface.models.ActiveClass.ActiveClassS import ActiveClassS
from py_dss_interface.models.ActiveClass.ActiveClassV import ActiveClassV


class ActiveClass(ActiveClassS, ActiveClassI, ActiveClassV):
    """This interface implements the ActiveClass (IActiveClass) interface of OpenDSS by declaring 3 procedures for
    accessing the different properties included in this interface: ActiveClassS, ActiveClassI, ActiveClassV. Ih the
    original paper Davis cited that are 4 procedures, but only 3 were described."""
    pass
