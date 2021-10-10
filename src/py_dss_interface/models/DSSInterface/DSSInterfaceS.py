# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSInterfaceS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr DSSS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def dss_new_circuit(self, argument: str) -> str:
        """Makes a new circuit, the name of the circuit must be specified in the Argument."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dss_version(self) -> str:
        """Gets the version string for the DSS."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def dss_read_datapath(self) -> str:
        """Gets the Data File Path. Default for reports, etc. from DSS."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def dss_write_datapath(self, argument: str) -> str:
        """Sets the Data File Path. Default for reports, etc. from DSS."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(3), argument.encode('ascii')))
        result = result.value.decode('ascii')
        if result == '0':
            print("Path writen succesfully!")
        return result

    def dss_default_editor(self) -> str:
        """Gets the path name for the default text editor."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')
