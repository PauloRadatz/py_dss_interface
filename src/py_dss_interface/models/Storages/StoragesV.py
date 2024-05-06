# -*- coding: utf-8 -*-
# @Time    : 5/6/2024 7:32 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : StoragesV.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List


class StoragesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SwtControlsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _names(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.StoragesV, 0)

    def _register_names(self) -> List[str]:
        return Bridge.pointer_read(self._dss_obj.StoragesV, 1)

    def _register_values(self) -> List[float]:
        return Bridge.pointer_read(self._dss_obj.StoragesV, 2)
