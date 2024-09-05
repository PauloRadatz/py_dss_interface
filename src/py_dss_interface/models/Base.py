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
