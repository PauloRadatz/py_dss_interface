# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class RegControlsS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr RegControlsS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def regcontrols_read_name(self):
        """Gets the active RegControl name."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_name(self, argument):
        """Sets the active RegControl name."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_monitored_bus(self):
        """Gets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_monitored_bus(self, argument):
        """Sets the name of the remote regulated bus, in lieu of LDC settings."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_transformer(self):
        """Gets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_transformer(self, argument):
        """Sets the name of the transformer this regulator controls."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')
