# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CircuitS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr CircuitS(int32_t Parameter, CStr Argument);

    This interface returns a string according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _name(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.CircuitS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TODO: Must be reimplemented and reviewed
    def _disable(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.CircuitS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TODO: must be reimplemented and reviewed
    def _enable(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.CircuitS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def _set_active_element(self, argument: str) -> str:
        argument = Base._check_string_param(argument, default="")
        result = ctypes.c_char_p(self._dss_obj.CircuitS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _set_active_class(self, argument: str) -> str:
        argument = Base._check_string_param(argument, default="")
        result = ctypes.c_char_p(self._dss_obj.CircuitS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def _set_active_bus(self, argument: str) -> str:
        argument = Base._check_string_param(argument, default="")
        result = ctypes.c_char_p(self._dss_obj.CircuitS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')
