# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class PVSystemsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double PVSystemsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.

    """

    def pvsystems_read_irradiance(self) -> float:
        """Gets the present value of the Irradiance property in W/sq-m."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def pvsystems_write_irradiance(self, argument) -> float:
        """Sets the present value of the Irradiance property in W/sq-m."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def pvsystems_kw(self) -> float:
        """Gets the kW output."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def pvsystems_read_kvar(self) -> float:
        """Gets the kvar value."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(3), ctypes.c_double(0)))

    def pvsystems_write_kvar(self, argument) -> float:
        """Sets the kvar value."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(4), ctypes.c_double(argument)))

    def pvsystems_read_pf(self) -> float:
        """Gets the power factor value."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(5), ctypes.c_double(0)))

    def pvsystems_write_pf(self, argument) -> float:
        """Sets the power factor value."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(6), ctypes.c_double(argument)))

    def pvsystems_read_kva_rated(self) -> float:
        """Gets the rated kVA of the PVSystem."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(7), ctypes.c_double(0)))

    def pvsystems_write_kva_rated(self, argument) -> float:
        """Sets the rated kVA of the PVSystem."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(8), ctypes.c_double(argument)))

    def pvsystems_read_pmpp(self) -> float:
        """Gets the Pmpp."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(9), ctypes.c_double(0)))

    def pvsystems_write_pmpp(self, argument) -> float:
        """Sets the Pmpp."""
        return float(self.dss_obj.PVsystemsF(ctypes.c_int32(10), ctypes.c_double(argument)))

    # TODO there are not those guys in the DLL
    # def pvsystems_read_kv(self) -> float:
    #     """Gets the kV."""
    #     return float(self.dss_obj.PVsystemsF(ctypes.c_int32(11), ctypes.c_double(0)))
    #
    # def pvsystems_write_kv(self, argument) -> float:
    #     """Sets the kV."""
    #     return float(self.dss_obj.PVsystemsF(ctypes.c_int32(12), ctypes.c_double(argument)))
