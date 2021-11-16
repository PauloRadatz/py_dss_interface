# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models.Base import Base


class SwtControlsS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr SwtControlsS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def swtcontrols_read_name(self):
        """Gets the active swtcontrol name."""
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def swtcontrols_write_name(self, argument):
        """Sets the active swtcontrol by name."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def swtcontrols_read_switched_obj(self):
        """Gets the name of the switched object by the active SwtControl """
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def swtcontrols_write_switched_obj(self, argument):
        """Sets the switched object by name."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SwtControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')
