# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class TransformersF(Base):
    """
    This interface can be used to read/modify the properties of the Transformers Class where the values are doubles.

    The structure of the interface is as follows:
        double TransformersF(int32_t Parameter, double argument) ;

    This interface returns a floating point number (64 bits), the variable “parameter” is used to specify the
    property of the class to be used and the variable “argument” can be used to modify the value of the property when
    necessary. Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def transformers_read_r(self) -> float:
        """Gets the active winding resistance in %."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(0), ctypes.c_double(0)))

    def transformers_write_r(self, argument) -> float:
        """Sets the active winding resistance in %."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def transformers_read_tap(self) -> float:
        """Gets the active winding tap in per-unit."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(2), ctypes.c_double(0)))

    def transformers_write_tap(self, argument) -> float:
        """Sets the active winding tap in per-unit."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def transformers_read_min_tap(self) -> float:
        """Gets the active winding minimum tap in per-unit."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(4), ctypes.c_double(0)))

    def transformers_write_min_tap(self, argument) -> float:
        """Sets the active winding minimum tap in per-unit."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def transformers_read_max_tap(self) -> float:
        """Gets the active winding maximum tap in per-unit."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(6), ctypes.c_double(0)))

    def transformers_write_max_tap(self, argument) -> float:
        """Sets the active winding maximum tap in per-unit."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def transformers_read_kv(self) -> float:
        """Gets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(8), ctypes.c_double(0)))

    def transformers_write_kv(self, argument) -> float:
        """Sets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def transformers_read_kva(self) -> float:
        """Gets the active winding kVA rating. On winding 1, this also determines normal and emergency current
        ratings for all windings. """
        return float(self.dss_obj.TransformersF(ctypes.c_int32(10), ctypes.c_double(0)))

    def transformers_write_kva(self, argument) -> float:
        """Sets the active winding kVA rating. On winding 1, this also determines normal and emergency current
        ratings for all windings. """
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def transformers_read_x_neut(self) -> float:
        """Gets the active winding neutral reactance [ohms] for wye connections."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(12), ctypes.c_double(0)))

    def transformers_write_x_neut(self, argument) -> float:
        """Sets the active winding neutral reactance [ohms] for wye connections."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def transformers_read_r_neut(self) -> float:
        """Gets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(14), ctypes.c_double(0)))

    def transformers_write_r_neut(self, argument) -> float:
        """Sets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(15), ctypes.c_double(argument)))

    def transformers_read_xhl(self) -> float:
        """Gets the percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2 winding or 3 winding
        transformers. """
        return float(self.dss_obj.TransformersF(ctypes.c_int32(16), ctypes.c_double(0)))

    def transformers_write_xhl(self, argument) -> float:
        """Sets the percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2 winding or 3 winding
        transformers. """
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(17), ctypes.c_double(argument)))

    def transformers_read_xht(self) -> float:
        """Gets the percent reactance between windings 1 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only."""
        return float(self.dss_obj.TransformersF(ctypes.c_int32(18), ctypes.c_double(0)))

    def transformers_write_xht(self, argument) -> float:
        """Sets the percent reactance between windings 1 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only. """
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(19), ctypes.c_double(argument)))

    def transformers_read_xlt(self) -> float:
        """Gets he percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only. """
        return float(self.dss_obj.TransformersF(ctypes.c_int32(20), ctypes.c_double(0)))

    def transformers_write_xlt(self, argument) -> float:
        """Sets the percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only. """
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.TransformersF(ctypes.c_int32(21), ctypes.c_double(argument)))
