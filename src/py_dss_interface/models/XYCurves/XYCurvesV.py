# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Text.Text import Text
from py_dss_interface.models.XYCurves.XYCurvesS import XYCurvesS


class XYCurvesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void XYCurvesS(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def xycurves_read_x_array(self):
        """Gets the X values as a variant array of doubles. Set Npts to max number expected if setting."""
        return Bridge.var_array_function(self.dss_obj.XYCurvesV, ctypes.c_int(0), ctypes.c_int(0), None)

    def xycurves_write_x_array(self, argument):
        """Sets the X values as a variant array of doubles specified in Argument. Set Npts to max number expected
        if setting."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        xyc = XYCurvesS(self.dss_obj)
        xyc_name = xyc.xycurves_read_name()
        return t.text(f'edit XYCurve.{xyc_name} Xarray = {argument}')

    def xycurves_read_y_array(self):
        """Gets the Y values as a variant array of doubles. Set Npts to max number expected if setting.."""
        return Bridge.var_array_function(self.dss_obj.XYCurvesV, ctypes.c_int(2), ctypes.c_int(0), None)

    def xycurves_write_y_array(self, argument):
        """Sets the Y values as a variant array of doubles specified in Argument. Set Npts to max number expected
        if setting."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        xyc = XYCurvesS(self.dss_obj)
        xyc_name = xyc.xycurves_read_name()
        return t.text(f'edit XYCurve.{xyc_name} Yarray = {argument}')

        # variant_pointer = ctypes.pointer(automation.VARIANT())
        # variant_pointer.contents.value = argument
        # self.dss_obj.XYCurvesV(ctypes.c_int(3), variant_pointer)
        # return variant_pointer.contents.value
