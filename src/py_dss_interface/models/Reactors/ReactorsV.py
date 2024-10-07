# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 8:28 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : ReactorsV.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List


class ReactorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ReactorsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _names(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 0)

    def _rmatrix_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 1)

    def _rmatrix_write(self, arg: List[float]) -> List[int]:
        return Bridge.pointer_write(self._dss_obj.ReactorsV, 2, arg, 2)

    def _xmatrix_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 3)

    def _xmatrix_write(self, arg: List[float]) -> List[int]:
        return Bridge.pointer_write(self._dss_obj.ReactorsV, 4, arg, 2)

    def _z_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 5)

    def _z_write(self, arg: List[float]) -> List[int]:
        return Bridge.pointer_write(self._dss_obj.ReactorsV, 6, arg, 2)

    def _z0_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 7)

    def _z0_write(self, arg: List[float]) -> List[int]:
        return Bridge.pointer_write(self._dss_obj.ReactorsV, 8, arg, 2)

    def _z1_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 9)

    def _z1_write(self, arg: List[float]) -> List[int]:
        return Bridge.pointer_write(self._dss_obj.ReactorsV, 10, arg, 2)

    def _z2_read(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.ReactorsV, 11)

    def _z2_write(self, arg: List[float]) -> List[int]:
        return Bridge.pointer_write(self._dss_obj.ReactorsV, 12, arg, 2)
