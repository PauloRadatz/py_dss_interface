# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class TransformersI(Base):
    """
    This interface can be used to read/modify the properties of the Transformers Class where the values are integers.

    The structure of the interface is as follows:
        int32_t TransformersI(int32_t Parameter, int32_t argument) ;

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def transformers_read_num_windings(self) -> int:
        """Gets the number of windings on this transformer. Allocates memory; set or change this property first."""
        return self.dss_obj.TransformersI(ctypes.c_int32(0), ctypes.c_int32(0))

    def transformers_write_num_windings(self, argument) -> int:
        """Sets the number of windings on this transformer. Allocates memory; set or change this property first."""
        return self.dss_obj.TransformersI(ctypes.c_int32(1), ctypes.c_int32(argument))

    def transformers_read_wdg(self) -> int:
        """Gets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        return self.dss_obj.TransformersI(ctypes.c_int32(2), ctypes.c_int32(0))

    def transformers_write_wdg(self, argument) -> int:
        """Sets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        return self.dss_obj.TransformersI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def transformers_read_num_taps(self) -> int:
        """Gets the active winding number of tap steps between MinTap and MaxTap."""
        return self.dss_obj.TransformersI(ctypes.c_int32(4), ctypes.c_int32(0))

    def transformers_write_num_taps(self, argument) -> int:
        """Sets the active winding number of tap steps between MinTap and MaxTap."""
        return self.dss_obj.TransformersI(ctypes.c_int32(5), ctypes.c_int32(argument))

    def transformers_read_is_delta(self) -> int:
        """Gets the information about if the active winding is delta (1) or wye (0) connection."""
        return self.dss_obj.TransformersI(ctypes.c_int32(6), ctypes.c_int32(0))

    def transformers_write_is_delta(self, argument) -> int:
        """Sets the information about if the active winding is delta (1) or wye (0) connection."""
        return self.dss_obj.TransformersI(ctypes.c_int32(7), ctypes.c_int32(argument))

    def transformers_first(self) -> int:
        """Sets the first Transformer active. Return 0 if no more."""
        return self.dss_obj.TransformersI(ctypes.c_int32(8), ctypes.c_int32(0))

    def transformers_next(self) -> int:
        """Sets the next Transformer active. Return 0 if no more."""
        return self.dss_obj.TransformersI(ctypes.c_int32(9), ctypes.c_int32(0))

    def transformers_count(self) -> int:
        """Gets the number of transformers within the active circuit."""
        return self.dss_obj.TransformersI(ctypes.c_int32(10), ctypes.c_int32(0))
