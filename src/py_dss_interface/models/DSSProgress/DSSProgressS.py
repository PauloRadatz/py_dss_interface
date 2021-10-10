# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSProgressS(Base):
    """
    TThis interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr DSSProgressS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def dssprogress_caption(self, arg: str) -> str:
        """Sets the caption to appear on the bottom of the DSS Progress form."""

        arg = Base.check_string_param(arg)
        ctypes.c_char_p(arg.encode('utf-8'))
        return (self.dss_obj.DSSProgressS(0, ctypes.c_char_p(arg.encode('utf-8')))).decode('ascii')
