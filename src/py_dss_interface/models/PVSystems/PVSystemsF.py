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

    def _irradiance_read(self) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(0), ctypes.c_double(0)))

    def _irradiance_write(self, argument) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def _kw(self) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def _kvar_read(self) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(3), ctypes.c_double(0)))

    def _kvar_write(self, argument) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(4), ctypes.c_double(argument)))

    def _pf_read(self) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(5), ctypes.c_double(0)))

    def _pf_write(self, argument) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(6), ctypes.c_double(argument)))

    def _kva_rated_read(self) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(7), ctypes.c_double(0)))

    def _kva_rated_write(self, argument) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(8), ctypes.c_double(argument)))

    def _pmpp_read(self) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(9), ctypes.c_double(0)))

    def _pmpp_write(self, argument) -> float:
        return float(self._dss_obj.PVsystemsF(ctypes.c_int32(10), ctypes.c_double(argument)))

    # TODO there are not those guys in the DLL
    # def _kv(self) -> float:
    #     """Gets the kV."""
    #     return float(self.dss_obj.PVsystemsF(ctypes.c_int32(11), ctypes.c_double(0)))
    #
    # def _kv_write(self, argument) -> float:
    #     """Sets the kV."""
    #     return float(self.dss_obj.PVsystemsF(ctypes.c_int32(12), ctypes.c_double(argument)))
