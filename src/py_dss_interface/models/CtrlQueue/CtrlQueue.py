# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CtrlQueue(Base):
    # CtrlQueueI(int)
    def ctrlqueue_clearqueue(self):
        """Clears the control queue."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def ctrlqueue_delete(self):
        """Deletes a control action from the DSS control queue by referencing the handle of the action (Argument)."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def ctrlqueue_numactions(self):
        """Gets the number of actions on the current action list (that have been popped off the control queue by
        CheckControlActions)."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def ctrlqueue_action(self):
        """Sets the active action by index (argument)."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def ctrlqueue_actioncode(self):
        """Gets the code for the active action. Long integer code to tell the control device what to do."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def ctrlqueue_devicehandle(self):
        """Gets the handle (user defined) to device that must act on the pending action."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def ctrlqueue_push(self):
        """Pushes a control action onto the DSS control queue by time, action code, and device handle.
        Returns Control Queue handle."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def ctrlqueue_show(self):
        """Shows the entire control queue in CSV format."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def ctrlqueue_clearactions(self):
        """Clears the action list."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def ctrlqueue_popaction(self):
        """Pops next action off the action list and makes it the active action. Returns zero if none."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def ctrlqueue_queuesize(self):
        """Delivers the size of the current control queue. Returns zero if none."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def ctrlqueue_doallqueue(self):
        """Forces the execution of all control actions stored at the control queue. Returns 0."""
        result = self.dss_obj.CtrlQueueI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    # CtrlQueueV (Variant)
    def ctrlqueue_ctrlqueue(self):
        """Delivers the control actions contained in the CtrlQueue after the latest solve command."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CtrlQueueV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
