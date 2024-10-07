# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 8:28 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : ReactorsI.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models.Base import Base


class ReactorsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t ReactorsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _first(self) -> int:
        return self._dss_obj.ReactorsI(0, 0)

    def _next(self) -> int:
        return self._dss_obj.ReactorsI(1, 0)

    def _count(self) -> int:
        return self._dss_obj.ReactorsI(2, 0)

    def _parallel(self) -> int:
        return self._dss_obj.ReactorsI(3, 0)

    def _parallel_write(self, arg: int) -> int:
        return self._dss_obj.ReactorsI(4, arg)
