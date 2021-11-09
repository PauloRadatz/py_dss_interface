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
    def error_code(self) -> int:
        """"This interface returns an integer with latest error code delivered by OpenDSS."""
        return int(self.dss_obj.ErrorCode())
