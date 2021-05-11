# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class ErrorDesc(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr ErrorDesc(void );

    This interface returns a string with description of the latest error code delivered by OpenDSS.
    """

    def error_desc(self) -> str:
        """"This interface returns an integer with latest error code delivered by OpenDSS."""
        return str(self.dss_obj.DSSErrorDesc())
