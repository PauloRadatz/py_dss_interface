# -*- coding: utf-8 -*-
# @Time    : 5/6/2024 7:32 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : StoragesS.py
# @Software: PyCharm

import ctypes

from py_dss_interface.models.Base import Base


class StoragesS(Base):

    def _name_read(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.StoragesS(ctypes.c_int32(0), "".encode('ascii')))
        return result.value.decode('ascii')

    def _name_write(self, argument: str) -> str:
        result = ctypes.c_char_p(self._dss_obj.StoragesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')
