# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class XYCurves(Base):
    """
    This interface implements the XYCurves (IXYCurves) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    # XYCurves (int)
    def count(self):
        """Gets number of XYCurves in active circuit."""
        result = self.dss_obj.XYCurveI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def first(self):
        """Sets first XYCurves object active; returns 0 if none."""
        result = self.dss_obj.XYCurveI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def next(self):
        """Sets next XYCurves object active; returns 0 if none."""
        result = self.dss_obj.XYCurveI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def read_npts(self):
        """Gets the number of points in X-Y curve."""
        result = self.dss_obj.XYCurveI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def write_npts(self, argument):
        """Sets the number of points in X-Y curve."""
        result = self.dss_obj.XYCurveI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    # XYCurvesF (Float)
    def read_x(self):
        """Gets the interpolated value after setting Y."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def write_x(self, argument):
        """Sets the X value."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def read_y(self):
        """Gets the interpolated value after setting X."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def write_y(self, argument):
        """Sets the Y value."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def read_xshift(self):
        """Gets the amount to shift X value from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def write_xshift(self, argument):
        """Sets the amount to shift X value from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def read_yshift(self):
        """Gets the amount to shift Y value from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def write_yshift(self, argument):
        """Sets the amount to shift Y value from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def read_xscale(self):
        """Gets the factor to scale X values from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def write_xscale(self, argument):
        """Sets the factor to scale X values from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def read_yscale(self):
        """Gets the factor to scale Y values from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def write_yscale(self, argument):
        """Sets the factor to scale Y values from original curve."""
        result = float(self.dss_obj.XYCurvesF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    # XYCurvesS (String)
    def read_name(self):
        """Gets the name of the active XYCurve Object."""
        result = ctypes.c_char_p(self.dss_obj.XYCurvesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def write_name(self, argument):
        """Sets the name of the active XYCurve Object."""
        result = ctypes.c_char_p(self.dss_obj.XYCurvesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # XYCurvesV (Variant)
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
