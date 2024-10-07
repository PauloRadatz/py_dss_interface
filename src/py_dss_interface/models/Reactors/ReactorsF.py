# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 8:27 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : ReactorsF.py
# @Software: PyCharm

# -*- coding: iso-8859-15 -*-

import ctypes

from py_dss_interface.models.Base import Base


class ReactorsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double ReactorsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def _kv(self) -> float:
        return self._dss_obj.ReactorsF(0, 0)

    def _kv_write(self, arg: float):
        return self._dss_obj.ReactorsF(1, ctypes.c_double(arg))

    def _kvar(self) -> float:
        return self._dss_obj.ReactorsF(2, 0)

    def _kvar_write(self, arg: float):
        return self._dss_obj.ReactorsF(3, ctypes.c_double(arg))

    def _imh(self) -> float:
        return self._dss_obj.ReactorsF(4, 0)

    def _imh_write(self, arg: float):
        return self._dss_obj.ReactorsF(5, ctypes.c_double(arg))

    def _r(self) -> float:
        return self._dss_obj.ReactorsF(6, 0)

    def _r_write(self, arg: float):
        return self._dss_obj.ReactorsF(7, ctypes.c_double(arg))

    def _rp(self) -> float:
        return self._dss_obj.ReactorsF(8, 0)

    def _rp_write(self, arg: float):
        return self._dss_obj.ReactorsF(9, ctypes.c_double(arg))

    def _x(self) -> float:
        return self._dss_obj.ReactorsF(10, 0)

    def _x_write(self, arg: float):
        return self._dss_obj.ReactorsF(11, ctypes.c_double(arg))
