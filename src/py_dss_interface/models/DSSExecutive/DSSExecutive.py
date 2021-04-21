# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class DSSExecutive(Base):

    # DSS_ExecutiveI (int)
    def executive_numcommands(self):
        """Gets the number of DSS Executive Commands."""
        result = int(self.dss_obj.DSSExecutiveI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def executive_numoptions(self):
        """Gets the number of DSS Executive Options."""
        result = int(self.dss_obj.DSSExecutiveI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    # DSSExecutiveS (String)
    def executive_command(self):
        """Gets i-th command (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dss_obj.DSSExecutiveS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_option(self):
        """Gets i-th option (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dss_obj.DSSExecutiveS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_commandhelp(self):
        """Gets help string for i-th command (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dss_obj.DSSExecutiveS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_optionhelp(self):
        """Gets help string for i-th option (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dss_obj.DSSExecutiveS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_optionvalue(self):
        """Gets present value for i-th option (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dss_obj.DSSExecutiveS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')
