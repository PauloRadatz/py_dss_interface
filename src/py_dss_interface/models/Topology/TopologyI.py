# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class TopologyI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t TopologyI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _num_loops(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _num_isolated_branches(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _num_isolated_loads(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _first(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(3), ctypes.c_int32(0))

    def _next(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(4), ctypes.c_int32(0))

    def _active_branch(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _forward_branch(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(6), ctypes.c_int32(0))

    def _backward_branch(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _looped_branch(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(8), ctypes.c_int32(0))

    def _parallel_branch(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _first_load(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _next_load(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(11), ctypes.c_int32(0))

    def _active_level(self) -> int:
        return self._dss_obj.TopologyI(ctypes.c_int32(12), ctypes.c_int32(0))
