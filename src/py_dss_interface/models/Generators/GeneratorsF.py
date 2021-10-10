# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class GeneratorsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double GeneratorsF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def generators_read_kv(self) -> float:
        """Gets the voltage base for the active generator, kV."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def generators_write_kv(self, argument: float) -> float:
        """Sets the voltage base for the active generator, kV."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def generators_read_kw(self) -> float:
        """Gets the kW output for the active generator, kvar is updated for current power factor."""
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(2), ctypes.c_double(0)))

    def generators_write_kw(self, argument: float) -> float:
        """Sets the kW output for the active generator, kvar is updated for current power factor."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def generators_read_kvar(self) -> float:
        """Gets the kvar output for the active generator, kW is updated for current power factor."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def generators_write_kvar(self, argument: float) -> float:
        """Sets the kvar output for the active generator, kW is updated for current power factor."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def generators_read_pf(self) -> float:
        """Gets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(6), ctypes.c_double(0)))

    def generators_write_pf(self, argument: float) -> float:
        """Sets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def generators_read_kva_rated(self) -> float:
        """Gets the KVA rating of the generator."""
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(8), ctypes.c_double(0)))

    def generators_write_kva_rated(self, argument: float) -> float:
        """Sets the KVA rating of the generator."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def generators_read_vmax_pu(self) -> float:
        """Gets the Vmaxpu for Generator Model."""
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(10), ctypes.c_double(0)))

    def generators_write_vmax_pu(self, argument: float) -> float:
        """Sets the Vmaxpu for Generator Model."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def generators_read_vmin_pu(self) -> float:
        """Gets the Vminpu for Generator Model."""
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(12), ctypes.c_double(0)))

    def generators_write_vmin_pu(self, argument: float) -> float:
        """Sets the Vminpu for Generator Model."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.GeneratorsF(ctypes.c_int32(13), ctypes.c_double(argument)))
