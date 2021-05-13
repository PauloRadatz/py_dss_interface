# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class RegControls(Base):
    """
    This interface implements the RegControls (IRegControls) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: .
    """

    # RegControlsS (String)
    def regcontrols_read_name(self):
        """Gets the active RegControl name."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_name(self, argument):
        """Sets the active RegControl name."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_monitoredbus(self):
        """Gets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_monitoredbus(self, argument):
        """Sets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_transformer(self):
        """Gets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_transformer(self, argument):
        """Sets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # RegControlsV (Variant)
    def regcontrols_allnames(self):
        """Gets a variant array of strings containing all RegControl names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.RegControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
