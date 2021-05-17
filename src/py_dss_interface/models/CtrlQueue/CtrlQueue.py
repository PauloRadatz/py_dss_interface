# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.CtrlQueue.CtrlQueueI import CtrlQueueI
from py_dss_interface.models.CtrlQueue.CtrlQueueV import CtrlQueueV


class CtrlQueue(CtrlQueueI, CtrlQueueV):
    """
    This interface implements the CtrlQueue (ICtrlQueue) interface of OpenDSS by declaring 2 procedures for accessing
    the different properties included in this interface: CtrlQueueI, CtrlQueueV
    """
    pass
