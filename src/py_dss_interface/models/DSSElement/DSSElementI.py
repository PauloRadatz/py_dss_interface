# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSElementI(Base):
    """
    This interface can be used to read/modify the properties of the DSSElement Class where the values are integers.

    The structure of the interface is as follows:
        int32_t DSSElementI(int32_t Parameter, int32_t argument) ;

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _num_properties(self) -> int:
        return int(self._dss_obj.DSSElementI(ctypes.c_int32(0), ctypes.c_int32(0)))
