# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LinesI(Base):
    """
    This interface can be used to read/modify the properties of the Lines Class where the values are integers.

    The structure of the interface is as follows:
        int32_t LinesI(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def lines_first(self) -> int:
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return self.dss_obj.LinesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def lines_next(self) -> int:
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return self.dss_obj.LinesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def lines_read_phases(self) -> int:
        """Gets the number of phases of the active line object."""
        return self.dss_obj.LinesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def lines_write_phases(self, argument: int) -> int:
        """Sets the number of phases of the active line object."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.LinesI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def lines_num_cust(self) -> int:
        """Gets the number of customers on this line section."""
        return self.dss_obj.LinesI(ctypes.c_int32(4), ctypes.c_int32(0))

    def lines_parent(self) -> int:
        """Gets the parents of the active Line to be the active Line. Return 0 if no parent or action fails."""
        return self.dss_obj.LinesI(ctypes.c_int32(5), ctypes.c_int32(0))

    def lines_count(self) -> int:
        """Gets the number of Line Objects in Active Circuit."""
        return self.dss_obj.LinesI(ctypes.c_int32(6), ctypes.c_int32(0))

    def lines_read_units(self) -> int:
        """Gets the units of the line (distance, check manual for details)."""
        return self.dss_obj.LinesI(ctypes.c_int32(7), ctypes.c_int32(0))

    def lines_write_units(self, argument: int) -> int:
        """Sets the units of the line (distance, check manual for details).
        units: {none | mi|kft|km|m|Ft|in|cm }
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
        return self.dss_obj.LinesI(ctypes.c_int32(8), ctypes.c_int32(argument))
