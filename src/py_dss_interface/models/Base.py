# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation


class Base:

    def __init__(self, obj_dss):
        self.dss_obj = obj_dss

    def get_string(self, first, second):
        result = ctypes.c_char_p(self.dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_int32(second)))
        return result.value.decode('ascii')

    def get_integer(self, first, second):
        # TODO: We need check the conditions below. Fist and Second must be >=0 ?
        if isinstance(first, int) and isinstance(second, int) and first >= 0 and second >= 0:
            return int(self.dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_int32(second)))
        else:
            return -1

    def get_variant(self, first):
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.type(self).__name__(ctypes.c_int32(first), variant_pointer)
        return variant_pointer.contents.value

    def get_float(self, first, second):
        # TODO: We need check the conditions below. Fist and Second must be >=0 ?
        if isinstance(first, int) and isinstance(second, int) and first >= 0 and second >= 0:
            return float(self.dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_double(second)))
        else:
            return -1
