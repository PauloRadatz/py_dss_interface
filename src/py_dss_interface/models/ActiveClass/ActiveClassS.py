# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Base import Base


class ActiveClassS(Base):
    """
    This interface can be used to read/modify the properties of the ActiveClass Class where the values are strings.

    The structure of the interface is as follows:
        CStr ActiveClassI(int32_t Parameter, CStr argument)

    This interface returns a string, the first parameter is used to specify the property of the class to be used
    and the second parameter can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _class_name(self) -> str:
        try:
            return (self._dss_obj.ActiveClassS(2, 0)).decode('ascii')
        except Exception as e:
            raise Exception(f"Error in _class_name: {e}")

    def _name(self) -> str:
        try:
            return (self._dss_obj.ActiveClassS(0, 0)).decode('ascii')
        except Exception as e:
            raise Exception(f"Error in _name: {e}")

    def _name_write(self, argument) -> str:
        try:
            return (self._dss_obj.ActiveClassS(1, argument.encode('ascii'))).decode('ascii')
        except Exception as e:
            raise Exception(f"Error in _name_write: {e}")

    def _parent_class_name(self) -> str:
        try:
            return (self._dss_obj.ActiveClassS(3, 0)).decode('ascii')
        except Exception as e:
            raise Exception(f"Error in _parent_class_name: {e}")
