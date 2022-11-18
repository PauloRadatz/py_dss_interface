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

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def clear_queue(self) -> int:
        return CtrlQueueI._clear_queue(self)

    @staticmethod # TODO check this one - I don't think it is a staticmethod
    def delete(argument: int) -> int:
        return CtrlQueueI._delete(argument)

    @property
    def num_actions(self) -> int:
        return CtrlQueueI._num_actions(self)

    def action(self, argument: int) -> int:
        return CtrlQueueI._action(self, argument)

    @property
    def action_code(self) -> int:
        return CtrlQueueI._action_code(self)

    @property
    def device_handle(self) -> int:
        return CtrlQueueI._device_handle(self)

    def push(self, arg):
        return CtrlQueueV._push(self, arg)

    @property
    def show(self) -> int:
        return CtrlQueueI._show(self)

    @property
    def clear_actions(self) -> int:
        return CtrlQueueI._clear_actions(self)

    @property
    def pop_action(self) -> int:
        return CtrlQueueI._pop_action(self)

    @property
    def queue_size(self) -> int:
        return CtrlQueueI._queue_size(self)

    @property
    def do_all_queue(self) -> int:
        return CtrlQueueI._do_all_queue(self)

    def ctrlqueue(self) -> str:
        return CtrlQueueV._ctrlqueue(self)
