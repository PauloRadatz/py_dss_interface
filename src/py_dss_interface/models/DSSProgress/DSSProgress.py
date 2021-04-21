# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class DSSProgress(Base):

    # DSSProgress Interface
    def dssprogress_pctprogress(self):
        """Sets the percent progress to indicate [0..100]."""
        result = int(self.dss_obj.DSSProgressI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def dssprogress_show(self):
        """Shows progress form with null caption and progress set to zero."""
        result = int(self.dss_obj.DSSProgressI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def dssprogress_close(self):
        """Closes (hides) DSS Progress form."""
        result = int(self.dss_obj.DSSProgressI(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result

    # DSSProgressS (String)
    def dssprogress_caption(self):
        """Sets the caption to appear on the bottom of the DSS Progress form."""
        result = ctypes.c_char_p(self.dss_obj.DSSProgressS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')
