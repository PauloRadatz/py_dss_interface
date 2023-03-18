# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class TopologyS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr TopologyS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _branch_name_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.TopologyS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _branch_name_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.TopologyS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _bus_name_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.TopologyS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TODO include in test
    def _bus_name_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.TopologyS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')
