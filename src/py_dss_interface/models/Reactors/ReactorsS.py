# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 8:28 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : ReactorsS.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models.Base import Base


class ReactorsS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr ReactorsS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _name(self) -> str:
        return (self._dss_obj.ReactorsS(0, 0)).decode('ascii')

    def _name_write(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.ReactorsS(ctypes.c_int32(1), arg.encode('ascii')))
        return result.value.decode('ascii')

    def _l_curve(self) -> str:
        return (self._dss_obj.ReactorsS(2, 0)).decode('ascii')

    def _l_curve_write(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.ReactorsS(ctypes.c_int32(3), arg.encode('ascii')))
        return result.value.decode('ascii')

    def _r_curve(self) -> str:
        return (self._dss_obj.ReactorsS(4, 0)).decode('ascii')

    def _r_curve_write(self, arg: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.ReactorsS(ctypes.c_int32(5), arg.encode('ascii')))
        return result.value.decode('ascii')
