# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class ErrorCode(Base):
    """
   This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t ErrorCode(void );
    """

    # TODO include in test
    def _error_code(self) -> int:
        return int(self._dss_obj.ErrorCode())
