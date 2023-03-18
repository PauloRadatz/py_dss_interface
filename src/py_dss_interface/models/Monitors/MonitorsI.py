# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MonitorsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t MonitorsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _first(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _next(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _reset(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _reset_all(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(3), ctypes.c_int32(0))

    def _sample(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(4), ctypes.c_int32(0))

    def _save(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _show(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(6), ctypes.c_int32(0))

    # TODO include in test
    def _mode(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _mode_write(self, argument: int) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(8), ctypes.c_int32(argument))

    def _sample_count(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _sample_all(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _save_all(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(11), ctypes.c_int32(0))

    def _count(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(12), ctypes.c_int32(0))

    def _process(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(13), ctypes.c_int32(0))

    def _process_all(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(14), ctypes.c_int32(0))

    def _file_version(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(15), ctypes.c_int32(0))

    def _record_size(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(16), ctypes.c_int32(0))

    def _num_channels(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(17), ctypes.c_int32(0))

    def _terminal(self) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(18), ctypes.c_int32(0))

    def _terminal_write(self, argument: int) -> int:
        return self._dss_obj.MonitorsI(ctypes.c_int32(19), ctypes.c_int32(argument))
