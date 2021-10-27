# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSExecutiveS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr DSSExecutiveS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def executive_command(self, arg: str) -> str:
        """Gets i-th command (specified in the argument as string)."""
        arg = Base.check_string_param(arg)
        ctypes.c_char_p(arg.encode('utf-8'))
        return (self.dss_obj.DSSExecutiveS(0, ctypes.c_char_p(arg.encode('utf-8')))).decode('ascii')

    def executive_option(self, arg: str) -> str:
        """Gets i-th option (specified in the argument as string)."""
        arg = Base.check_string_param(arg)
        ctypes.c_char_p(arg.encode('utf-8'))
        return (self.dss_obj.DSSExecutiveS(1, ctypes.c_char_p(arg.encode('utf-8')))).decode('ascii')

    def executive_command_help(self, arg: str) -> str:
        """Gets help string for i-th command (specified in the argument as string)."""
        arg = Base.check_string_param(arg)
        ctypes.c_char_p(arg.encode('utf-8'))
        return (self.dss_obj.DSSExecutiveS(2, ctypes.c_char_p(arg.encode('utf-8')))).decode('ascii')

    def executive_option_help(self, arg: str) -> str:
        """Gets help string for i-th option (specified in the argument as string)."""
        arg = Base.check_string_param(arg)
        ctypes.c_char_p(arg.encode('utf-8'))
        return (self.dss_obj.DSSExecutiveS(3, ctypes.c_char_p(arg.encode('utf-8')))).decode('ascii')

    def executive_option_value(self, arg: str) -> str:
        """Gets present value for i-th option (specified in the argument as string)."""
        arg = Base.check_string_param(arg)
        ctypes.c_char_p(arg.encode('utf-8'))
        return (self.dss_obj.DSSExecutiveS(4, ctypes.c_char_p(arg.encode('utf-8')))).decode('ascii')
