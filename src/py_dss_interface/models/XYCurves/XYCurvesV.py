# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class XYCurvesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void XYCurvesS(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def read_xarray(self):
        """Gets the X values as a variant array of doubles. Set Npts to max number expected if setting."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.XYCurvesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def write_xarray(self, argument):
        """Sets the X values as a variant array of doubles specified in Argument. Set Npts to max number expected
        if setting."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.XYCurvesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def read_yarray(self):
        """Gets the Y values as a variant array of doubles. Set Npts to max number expected if setting.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.XYCurvesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def write_yarray(self, argument):
        """Sets the Y values as a variant array of doubles specified in Argument. Set Npts to max number expected
        if setting."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.XYCurvesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value
