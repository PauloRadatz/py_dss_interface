# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class BusS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr BUSS(int32_t Parameter, CStr Argument)

    This interface returns a string according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _name(self) -> str:
        return (self._dss_obj.BUSS(0)).decode('ascii')
