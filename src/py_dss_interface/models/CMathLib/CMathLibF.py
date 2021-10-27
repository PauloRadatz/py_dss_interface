# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import numpy as np

from py_dss_interface.models.Base import Base


class CMathLibF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double CmathLibF(int32_t Parameter, double Argument1, double Argument2);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def cmathlib_cabs(self, arg_real: float, arg_imaginary: float) -> float:
        """Returns the absolute value of complex number given in real (Argument1) and imaginary (Argument2) doubles."""
        c_number = complex(arg_real, arg_imaginary)
        return float(abs(c_number))

    def cmathlib_cdang(self, arg_real: float, arg_imaginary: float) -> float:
        """Returns the angle, in degrees, of a complex number specified as two doubles: Real part (Argument1) and
        imaginary part (Argument2)."""
        c_number = complex(arg_real, arg_imaginary)
        return np.angle([c_number], deg=True)
