# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CtrlQueueI(Base):
    """
    This interface can be used to read/modify the properties of the CtrlQueue Class where the values are integers.

    The structure of the interface is as follows:
        int32_t CtrlQueueI(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def ctrlqueue_clear_queue(self) -> int:
        """Clears the control queue."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(0), ctypes.c_int32(0))

    def ctrlqueue_delete(self, argument: int) -> int:
        """Deletes a control action from the DSS control queue by referencing the handle of the action (Argument)."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(1), ctypes.c_int32(argument))

    def ctrlqueue_num_actions(self) -> int:
        """Gets the number of actions on the current action list (that have been popped off the control queue by
        CheckControlActions)."""
        try:
            return self.dss_obj.CtrlQueueI(ctypes.c_int32(2), ctypes.c_int32(0))
        except Exception as e:
            Base.warn_msg("An error occur when tried to get *Num Actions of CrlQueue* check if *Queue* is NOT empty", e)

    def ctrlqueue_action(self, argument: int) -> int:
        """Sets the active action by index (argument)."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def ctrlqueue_action_code(self) -> int:
        """Gets the code for the active action. Long integer code to tell the control device what to do."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(4), ctypes.c_int32(0))

    def ctrlqueue_device_handle(self) -> int:
        """Gets the handle (user defined) to device that must act on the pending action."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(5), ctypes.c_int32(0))

    def ctrlqueue_push(self) -> int:
        """Pushes a control action onto the DSS control queue by time, action code, and device handle. Returns
        Control Queue handle. """
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(6), ctypes.c_int32(0))

    def ctrlqueue_show(self) -> int:
        """Shows the entire control queue in CSV format."""
        try:
            return self.dss_obj.CtrlQueueI(ctypes.c_int32(7), ctypes.c_int32(0))
        except Exception as e:
            Base.warn_msg("An error occur when tried to get *Num Actions of CrlQueue* check if *Queue* is NOT empty", e)

    def ctrlqueue_clear_actions(self) -> int:
        """Clears the action list."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(8), ctypes.c_int32(0))

    def ctrlqueue_pop_action(self) -> int:
        """Pops next action off the action list and makes it the active action. Returns zero if none."""
        try:
            return self.dss_obj.CtrlQueueI(ctypes.c_int32(9), ctypes.c_int32(0))
        except Exception as e:
            Base.warn_msg("An error occur when tried to *Pop Next Action of CrlQueue* check if *Queue* is NOT empty", e)

    def ctrlqueue_queue_size(self) -> int:
        """Delivers the size of the current control queue. Returns zero if none."""
        return self.dss_obj.CtrlQueueI(ctypes.c_int32(10), ctypes.c_int32(0))

    def ctrlqueue_do_all_queue(self) -> int:
        """Forces the execution of all control actions stored at the control queue. Returns 0."""
        try:
            return self.dss_obj.CtrlQueueI(ctypes.c_int32(11), ctypes.c_int32(0))
        except Exception as e:
            Base.warn_msg("An error occur when tried to *Do All Control Actions of CrlQueue* check if *Queue* is NOT "
                          "empty", e)
