# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class ActiveClassI(Base):
    """This interface can be used to read/modify the properties of the ActiveClass Class where the values are
    integers.

    The structure of the interface is as follows:
        int32_t ActiveClassI(int32_t Parameter,int32_t argument).

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows."""

    def active_class_first(self) -> int:
        """Sets first element in the active class to be the active DSS object. If object is a CktElement,
        ActiveCktElement also points to this element. Returns 0 if none."""
        return self.dss_obj.ActiveClassI(0, 0)

    def active_class_next(self) -> int:
        """Sets next element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element. Returns 0 if none."""
        return self.dss_obj.ActiveClassI(1, 0)

    def active_class_num_elements(self) -> int:
        """Gets the number of elements in this class. Same as Count Property."""
        return self.dss_obj.ActiveClassI(2, 0)

    def active_class_count(self) -> int:
        """Gets the number of elements in this class. Same as NumElements Property."""
        return self.dss_obj.ActiveClassI(3, 0)
