# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.CtrlQueue.CtrlQueueI import CtrlQueueI
from py_dss_interface.models.CtrlQueue.CtrlQueueV import CtrlQueueV
from typing import List


class CtrlQueue(CtrlQueueI, CtrlQueueV):
    """
    This interface implements the CtrlQueue (ICtrlQueue) interface of OpenDSS by declaring 2 procedures for accessing
    the different properties included in this interface: CtrlQueueI, CtrlQueueV
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def clear_queue(self) -> int:
        """Clears the control queue."""
        return CtrlQueueI._clear_queue(self)

    def delete(self, argument: int) -> int:
        """Deletes a control action from the DSS control queue by referencing the handle of the action (Argument)."""
        return CtrlQueueI._delete(self, argument)

    @property
    def num_actions(self) -> int:
        """Gets the number of actions on the current action list (that have been popped off the control queue by
                CheckControlActions)."""
        return CtrlQueueI._num_actions(self)

    def action(self, argument: int) -> int:
        """Sets the active action by index (argument)."""
        return CtrlQueueI._action(self, argument)

    @property
    def action_code(self) -> int:
        """Gets the code for the active action. Long integer code to tell the control device what to do."""
        return CtrlQueueI._action_code(self)

    @property
    def device_handle(self) -> int:
        """Gets the handle (user defined) to device that must act on the pending action."""
        return CtrlQueueI._device_handle(self)

    def push(self, arg: List[float]) -> List[float]:
        """Pushes a control action onto the DSS control queue by time, action code, and device handle. Returns
                Control Queue handle. """
        return CtrlQueueV._push(self, arg)

    @property
    def show(self) -> int:
        """Shows the entire control queue in CSV format."""
        return CtrlQueueI._show(self)

    @property
    def clear_actions(self) -> int:
        """Clears the action list."""
        return CtrlQueueI._clear_actions(self)

    @property
    def pop_action(self) -> int:
        """Pops next action off the action list and makes it the active action. Returns zero if none."""
        return CtrlQueueI._pop_action(self)

    @property
    def queue_size(self) -> int:
        """Delivers the size of the current control queue. Returns zero if none."""
        return CtrlQueueI._queue_size(self)

    def do_all_queue(self) -> int:
        """Forces the execution of all control actions stored at the control queue. Returns 0."""
        return CtrlQueueI._do_all_queue(self)

    @property
    def ctrlqueue(self) -> List[str]:
        """Delivers the control actions contained in the CtrlQueue after the latest solve command."""
        return CtrlQueueV._ctrlqueue(self)
