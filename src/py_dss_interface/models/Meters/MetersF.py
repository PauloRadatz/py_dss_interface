# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class MetersF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double MetersF(int32_t Parameter, double Argument);

    This interface returns a floating point number (64 bits) according to the number sent in the variable
    “parameter”. The parameter can be one of the following.
    """

    def meters_saifi(self) -> float:
        """Returns SAIFI for this meter's zone. Execute reliability calc method first."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(0), ctypes.c_double(0)))

    def meters_saifikw(self) -> float:
        """Returns the SAIFI based on kW rather than number of customers. Get after reliability calcs."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(1), ctypes.c_double(0)))

    def meters_saidi(self) -> float:
        """Returns the SAIDI for this meter zone. Execute DoreliabilityCalc first."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(2), ctypes.c_double(0)))

    def meters_custinterrupts(self) -> float:
        """Returns the total customer interruptions for this meter zone based on reliability calcs."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(3), ctypes.c_double(0)))

    def meters_avgrepairtime(self) -> float:
        """Returns the average Repair Time in this Section of the meter zone."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(4), ctypes.c_double(0)))

    def meters_faultratexrepairhrs(self) -> float:
        """Returns the sum of Fault Rate Time Repair Hours in this section of the meter zone."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(5), ctypes.c_double(0)))

    def meters_sumbranchfltrates(self) -> float:
        """Returns the sum of the branch fault rates in this section of the meter's zone."""
        return float(self.dss_obj.MetersF(ctypes.c_int32(6), ctypes.c_double(0)))
