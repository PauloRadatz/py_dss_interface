# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class ErrorDesc(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr ErrorDesc(void );

    This interface returns a string with description of the latest error code delivered by OpenDSS.
    """

    # TODO include in test
    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/12
    def _error_desc(self) -> str:
        result = ctypes.c_char_p(self._dss_obj.ErrorDesc())
        return result.value.decode('ascii')
