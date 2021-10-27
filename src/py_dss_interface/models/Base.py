# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
import warnings
from typing import Optional

from colorama import Fore, Back


class Base:

    def __init__(self, obj_dss):
        self.dss_obj = obj_dss

    def get_string(self, first, second):
        result = ctypes.c_char_p(self.dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_int32(second)))
        return result.value.decode('ascii')

    def get_integer(self, first: int, second: int) -> int:
        first = Base.check_int_param(first)
        second = Base.check_int_param(second)
        return int(self.dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_int32(second)))

    # def get_variant(self, first):
    #     variant_pointer = ctypes.pointer(automation.VARIANT())
    #     self.dss_obj.type(self).__name__(ctypes.c_int32(first), variant_pointer)
    #     return variant_pointer.contents.value

    def get_float(self, first, second):
        first = Base.check_int_param(first)
        second = Base.check_float_param(second)
        return float(self.dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_double(second)))

    @classmethod
    def check_assertion_result(cls, param: int, message_1: Optional[str] = None, message_2: Optional[str] = None,
                               expected_value=0) -> None:
        """
            Check if the method converges to a expected result.
            :param param: value compared
            :param expected_value value expected, most cases is 0 but in others could be 1
            :param message_1: a generic message if assertion fails
            :param message_2: a more especific message could be appear when AsserionError raise
            :return:
            """
        if not isinstance(expected_value, int):
            expected_value = 0
        try:
            assert param == expected_value, message_1
        except AssertionError:
            warnings.warn(message_2, Warning)

    @classmethod
    def check_int_param(cls, param: int, default=0) -> int:
        """
        Check if the parameter is a int and if it exists. If not exist or if it isn't a int it will be 0.
        :param default: self explained
        :param param: any int number
        """
        if type(param) is None:
            print("Param not defined, param is 0")
            param = default
        elif type(param) is str:
            param = default
        elif type(param) is float:
            print("Your parameter will be truncated")
            param = int(param)
        elif type(param) is int:
            return param
        else:
            param = default
        return param

    @classmethod
    def check_float_param(cls, param: float, default=0.0) -> float:
        """
        Check if the parameter is a float and if it exists. If not exist or if it isn't a float it will be 0.0.
        :param default: self explained
        :param param: any float number, positive or negative or just 0.0
        """
        if type(param) is None or type(param) is str:
            print("Param not defined, param is 0.0")
            param = default
        elif type(param) is int:
            param = float(param)
        elif type(param) is float:
            return param
        else:
            param = default
        return param

    @classmethod
    def check_string_param(cls, param: str, default='NAME_DEFAULT') -> str:
        """
        Check if the parameter is a string and if it exists. If not exist or if it isn't a str it will be NAME_DEFAULT.
        :param default: self explained
        :param param: any str
        """
        if not isinstance(default, str):
            default = 'NAME_DEFAULT'
        if param is None or not isinstance(param, str):
            param = default
        return param

    @classmethod
    def warn_msg(cls, msg, error):
        count_ = msg.count("*")
        it = count_ / 2
        for _ in range(int(it)):
            msg = msg.replace("*", "\033[1m", 1)
            msg = msg.replace("*", "\033[22m", 1)
        print(f'{Back.YELLOW}{Fore.BLACK} {msg}:{Fore.RESET} {Back.RESET} {error.args}')
