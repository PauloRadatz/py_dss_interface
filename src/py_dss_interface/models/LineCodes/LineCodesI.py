# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LineCodesI(Base):
    """
    This interface implements the Lines (ILineCodes) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface:
    """

    """
    This interface can be used to read/modify the properties of the LineCode Class where the values are integers.

    The structure of the interface is as follows:
        int32_t LineCodesI(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def linecodes_count(self) -> int:
        """Gets the number of Line Objects in Active Circuit."""
        return self.dss_obj.LineCodesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def linecodes_first(self) -> int:
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return self.dss_obj.LineCodesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def linecodes_next(self) -> int:
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return self.dss_obj.LineCodesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def linecodes_read_units(self) -> int:
        """Delivers the units of the active LineCode as an integer."""
        return self.dss_obj.LineCodesI(ctypes.c_int32(3), ctypes.c_int32(0))

    def linecodes_write_units(self, argument: int) -> int:
        """Sets the units of the active LineCode. The units must be specified as an integer in the argument.
        Please refer to the OpenDSS User manual for more information.
        UNITS_MAXNUM =9;
        UNITS_NONE   =0;
        UNITS_MILES =1;
        UNITS_KFT   =2;
        UNITS_KM    =3;
        UNITS_M     =4;
        UNITS_FT    =5;
        UNITS_IN    =6;
        UNITS_CM    =7;
        UNITS_MM    =8;
        """
        argument = Base.check_int_param(argument)
        return self.dss_obj.LineCodesI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def linecodes_read_phases(self) -> int:
        """Delivers the number of phases of the active LineCode as an integer."""
        return self.dss_obj.LineCodesI(ctypes.c_int32(5), ctypes.c_int32(0))

    def linecodes_write_phases(self, argument) -> int:
        """Sets the number of phases of the active LineCode. The units must be specified as an integer in the
        argument. """
        return self.dss_obj.LineCodesI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def linecodes_is_z1z0(self) -> int:
        """Gets the flag (Boolean 1/0) denoting whether the impedance data were entered in symmetrical components."""
        return self.dss_obj.LineCodesI(ctypes.c_int32(7), ctypes.c_int32(0))
