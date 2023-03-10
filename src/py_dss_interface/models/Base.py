# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
import logging
import warnings
from typing import Optional

from colorama import Fore, Back


class Base:

    def __init__(self, obj_dss):
        self._dss_obj = obj_dss

    def _get_string(self, first, second):
        result = ctypes.c_char_p(self._dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_int32(second)))
        return result.value.decode('ascii')

    def _get_integer(self, first: int, second: int) -> int:
        first = Base._check_int_param(first)
        second = Base._check_int_param(second)
        return int(self._dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_int32(second)))

    def _get_float(self, first, second):
        first = Base._check_int_param(first)
        second = Base._check_float_param(second)
        return float(self._dss_obj.type(self).__name__(ctypes.c_int32(first), ctypes.c_double(second)))

    def _get_dss_obj(self):
        return self._dss_obj

    @classmethod
    def _check_assertion_result(cls, param: int, message_1: Optional[str] = None, message_2: Optional[str] = None,
                                expected_value=0) -> None:
        """
        Check if the method converges to an expected result.
        @param param: value compared
        @param expected_value value expected, most cases is 0 but in others could be 1
        @param message_1: a generic message if assertion fails
        @param message_2: a more specific message could be appeared when AssertionError raise
        :return:
        """
        if not isinstance(expected_value, int):
            expected_value = 0
        try:
            assert param == expected_value, message_1
        except AssertionError as e:
            raise AssertionError(message_2) from e

    @classmethod
    def _check_int_param(cls, int_param: int, default: int = 0) -> int:
        """
        Check if the parameter is an int and if it exists. If not exist or if it isn't an int it will be 0.
        @param default: default value to return if int_param is None or not an int
        @param int_param: any int number
        """
        if int_param is None:
            raise ValueError("int_param is None")
        elif not isinstance(int_param, (int, float)):
            logging.warning("int_param is not an int or a float, using default value %d", default)
            int_param = default
        elif isinstance(int_param, float):
            logging.warning("int_param is a float, converting to int (truncating decimal part)")
            int_param = int_param
        return int_param

    @classmethod
    def _check_float_param(cls, param: float, default: float = 0.0) -> float:
        """
        Check if the parameter is a float and if it exists. If not exist or if it isn't a float it will be 0.0.
        :param default: the default value to be used if the parameter is not a float
        :param param: any float number, positive or negative or just 0.0
        """
        if param is None or not isinstance(param, (int, float)):
            warnings.warn("WARNING: Param not defined, param is 0.0", Warning)
            param = default
        elif isinstance(param, int):
            param = param
        return param

    @classmethod
    def _check_string_param(cls, param: Optional[str], default: str = 'NAME_DEFAULT') -> str:
        """
        Check if the parameter is a string and if it exists. If not exist or if it isn't a str it will be NAME_DEFAULT.
        @param default_value: the default value to use if `param` is None or not a string
        @param param: the parameter to check
        """
        if not isinstance(default, str):
            default = 'NAME_DEFAULT'
        if param is None or not isinstance(param, str):
            param = default
        return param

    @classmethod
    def _warn_msg(cls, msg, error):
        if not isinstance(msg, str):
            raise TypeError("msg must be a string")
        if not isinstance(error, Exception):
            raise TypeError("error must be an instance of Exception")
        count_ = msg.count("*")
        it = count_ / 2
        for _ in range(int(it)):
            msg = msg.replace("*", "\033[1m", 1)
            msg = msg.replace("*", "\033[22m", 1)
        print(f'{Back.YELLOW}{Fore.BLACK} {msg}:{Fore.RESET} {Back.RESET} {error.args}')
