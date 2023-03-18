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

    # TODO lets not have try and except here

    def _clear_queue(self) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _delete(self, argument: int) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(1), ctypes.c_int32(argument))

    def _num_actions(self) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _action(self, argument: int) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def _action_code(self) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(4), ctypes.c_int32(0))

    def _device_handle(self) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _show(self) -> int:
        try:
            return self._dss_obj.CtrlQueueI(ctypes.c_int32(7), ctypes.c_int32(0))
        except Exception as e:
            Base._warn_msg("An error occur when tried to get *Num Actions of CrlQueue* check if *Queue* is NOT empty", e)

    def _clear_actions(self) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(8), ctypes.c_int32(0))

    def _pop_action(self) -> int:
        try:
            return self._dss_obj.CtrlQueueI(ctypes.c_int32(9), ctypes.c_int32(0))
        except Exception as e:
            Base._warn_msg("An error occur when tried to *Pop Next Action of CrlQueue* check if *Queue* is NOT empty", e)

    def _queue_size(self) -> int:
        return self._dss_obj.CtrlQueueI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _do_all_queue(self) -> int:
        try:
            return self._dss_obj.CtrlQueueI(ctypes.c_int32(11), ctypes.c_int32(0))
        except Exception as e:
            Base._warn_msg("An error occur when tried to *Do All Control Actions of CrlQueue* check if *Queue* is NOT "
                          "empty", e)
