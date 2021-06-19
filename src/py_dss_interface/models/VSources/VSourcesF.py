# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class VSourcesF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double VSourcesF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def vsources_read_base_kv(self) -> float:
        """Gets the source voltage in kV."""
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(0), ctypes.c_double(0)))

    def vsources_write_base_kv(self, argument) -> float:
        """Sets the source voltage in kV."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def vsources_read_pu(self) -> float:
        """Gets the source voltage in pu."""
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def vsources_write_pu(self, argument) -> float:
        """Sets the source voltage in pu."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def vsources_read_angle_deg(self) -> float:
        """Gets the source phase angle of first phase in degrees."""
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(4), ctypes.c_double(0)))

    def vsources_write_angle_deg(self, argument) -> float:
        """Sets the source phase angle of first phase in degrees."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def vsources_read_frequency(self) -> float:
        """Gets the source frequency in Hz."""
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(6), ctypes.c_double(0)))

    def vsources_write_frequency(self, argument) -> float:
        """Sets the source frequency in Hz."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.VsourcesF(ctypes.c_int32(7), ctypes.c_double(argument)))
