# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class ActiveClassI(Base):
    def _active_class_integer(self, first, second):
        return int(self.dss_obj.ActiveClassI(ctypes.c_int32(first), ctypes.c_int32(second)))

    # ActiveClassI (int)
    def active_class_first(self):
        """Sets first element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element. Returns 0 if none."""
        result = self.dss_obj.active_class_integer(0, 0)
        return result

    def active_class_next(self):
        """Sets next element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element. Returns 0 if none."""
        result = self.dss_obj.active_class_integer(1, 0)
        return result

    def active_class_num_elements(self):
        """Gets the number of elements in this class. Same as Count Property."""
        result = self.dss_obj.active_class_integer(2, 0)
        return result

    def active_class_count(self):
        """Gets the number of elements in this class. Same as NumElements Property."""
        result = self.dss_obj.active_class_integer(3, 0)
        return result
