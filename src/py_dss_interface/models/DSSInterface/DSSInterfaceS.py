# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSInterfaceS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr DSSS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _new_circuit(self, argument: str) -> str:
        argument = Base._check_string_param(argument)
        result = ctypes.c_char_p(self._dss_obj.DSSS(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _version(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _datapath_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _datapath_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSS(ctypes.c_int32(3), argument.encode('ascii')))
        result = result.value.decode('ascii')
        if result == '0':
            print("Path writen succesfully!")
        return result

    def _default_editor(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.DSSS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')
