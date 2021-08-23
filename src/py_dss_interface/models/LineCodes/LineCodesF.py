# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class LineCodesF(Base):
    """
    This interface can be used to read/modify the properties of the LineCode Class where the values are doubles.

    The structure of the interface is as follows:
        double LineCodesF(int32_t Parameter, double argument)

    This interface returns a floating point number, the variable “parameter” is used to specify the property of the
    class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def linecodes_read_r1(self) -> float:
        """Gets the Positive-sequence resistance in ohms per unit length for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(0), ctypes.c_double(0)))

    def linecodes_write_r1(self, argument: float) -> float:
        """Sets the Positive-sequence resistance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(1), ctypes.c_double(argument)))

    def linecodes_read_x1(self) -> float:
        """Gets the Positive-sequence reactance in ohms per unit length for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(2), ctypes.c_double(0)))

    def linecodes_write_x1(self, argument: float) -> float:
        """Sets the Positive-sequence reactance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(3), ctypes.c_double(argument)))

    def linecodes_read_r0(self) -> float:
        """Gets the Zero-sequence resistance in ohms per unit length for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(4), ctypes.c_double(0)))

    def linecodes_write_r0(self, argument: float) -> float:
        """Sets the Zero-sequence resistance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(5), ctypes.c_double(argument)))

    def linecodes_read_x0(self) -> float:
        """Gets the Zero-sequence reactance in ohms per unit length for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(6), ctypes.c_double(0)))

    def linecodes_write_x0(self, argument: float) -> float:
        """Sets the Zero-sequence reactance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(7), ctypes.c_double(argument)))

    def linecodes_read_c1(self) -> float:
        """Gets the Positive-sequence capacitance in nF per unit length for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(8), ctypes.c_double(0)))

    def linecodes_write_c1(self, argument: float) -> float:
        """Sets the Positive-sequence capacitance in nF per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(9), ctypes.c_double(argument)))

    def linecodes_read_c0(self) -> float:
        """Gets the Zero-sequence capacitance in ohms per unit length for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(10), ctypes.c_double(0)))

    def linecodes_write_c0(self, argument: float) -> float:
        """Sets the Zero-sequence capacitance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(11), ctypes.c_double(argument)))

    def linecodes_read_norm_amps(self) -> float:
        """Gets the normal ampere rating for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(12), ctypes.c_double(0)))

    def linecodes_write_norm_amps(self, argument: float) -> float:
        """Sets the normal ampere rating for the active LineCode. This value must be specified in the argument
        as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(13), ctypes.c_double(argument)))

    def linecodes_read_emerg_amps(self) -> float:
        """Gets the Emergency ampere rating for the active LineCode."""
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(14), ctypes.c_double(0)))

    def linecodes_write_emerg_amps(self, argument: float) -> float:
        """Sets the Emergency ampere rating for the active LineCode. This value must be specified in the argument
        as a double."""
        argument = Base.check_float_param(argument)
        return float(self.dss_obj.LineCodesF(ctypes.c_int32(15), ctypes.c_double(argument)))
