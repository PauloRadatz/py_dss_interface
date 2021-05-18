# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class RelaysS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr RelaysS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def relays_read_name(self):
        """Gets the name of the active Relay."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_name(self, argument):
        """Sets the name of the active Relay."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def relays_read_monitored_obj(self):
        """Gets the full name of the object this relay is monitoring."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_monitored_obj(self, argument):
        """Sets the full name of the object this relay is monitoring."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def relays_read_switched_obj(self):
        """Gets the full name of element that will switched when relay trips."""
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_switched_obj(self, argument):
        """Sets the full name of element that will switched when relay trips."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.RelaysS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')
