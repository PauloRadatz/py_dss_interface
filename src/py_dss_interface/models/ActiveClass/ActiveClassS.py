# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class ActiveClassS(Base):
    def active_class_string(self, first, second):
        return ctypes.c_char_p(self.dss_obj.ActiveClassS(ctypes.c_int32(first), ctypes.c_int32(second)))

    # ActiveClassS (String)
    def get_name(self):
        """Gets the name of the active Element of the Active class."""
        result = self.dss_obj.active_class_string(0, 0)
        return result.value.decode('ascii')

    def write_name(self, argument):
        """Sets the name of the active Element of the Active class."""
        result = ctypes.c_char_p(self.dss_obj.ActiveClassS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def set_name(self):
        """Sets the name of the active Element of the Active class."""
        result = self.dss_obj.active_class_string(2, 0)
        return result.value.decode('ascii')
