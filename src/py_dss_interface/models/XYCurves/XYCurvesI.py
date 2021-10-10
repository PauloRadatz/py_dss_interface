# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class XYCurvesI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t XYCurvesI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def xycurves_count(self) -> int:
        """Gets number of XYCurves in active circuit."""
        return self.dss_obj.XYCurvesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def xycurves_first(self) -> int:
        """Sets first XYCurves object active; returns 0 if none."""
        return self.dss_obj.XYCurvesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def xycurves_next(self) -> int:
        """Sets next XYCurves object active; returns 0 if none."""
        return self.dss_obj.XYCurvesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def xycurves_read_npts(self) -> int:
        """Gets the number of points in X-Y curve."""
        return self.dss_obj.XYCurvesI(ctypes.c_int32(3), ctypes.c_int32(0))

    def xycurves_write_npts(self, argument) -> int:
        """Sets the number of points in X-Y curve."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.XYCurvesI(ctypes.c_int32(4), ctypes.c_int32(argument))
