# -*- coding: iso-8859-15 -*-

import ctypes

from py_dss_interface.models.Base import Base


class ReclosersS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr ReclosersS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def reclosers_read_name(self):
        """Gets the name of the active Recloser Object."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_name(self, argument):
        """Sets the name of the active Recloser Object."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def reclosers_read_monitored_obj(self):
        """Gets the full name of object this Recloser is monitoring."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_monitored_obj(self, argument):
        """Sets the full name of object this Recloser is monitoring."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def reclosers_read_switched_obj(self):
        """Gets the full name of the circuit element that is being switched by this Recloser."""
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_switched_obj(self, argument):
        """Sets the full name of the circuit element that is being switched by this Recloser."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.ReclosersS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')
