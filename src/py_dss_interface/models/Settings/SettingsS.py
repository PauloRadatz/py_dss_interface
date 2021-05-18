# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class SettingsS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr SettingsS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def settings_read_auto_bus_list(self):
        """Gets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def settings_write_auto_bus_list(self, argument):
        """Sets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def settings_read_price_curve(self):
        """Gets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def settings_write_price_curve(self, argument):
        """Sets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.SettingsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')
