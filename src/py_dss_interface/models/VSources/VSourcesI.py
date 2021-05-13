# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class VSourcesI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t VSourcesI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def vsources_count(self) -> int:
        """Returns the number of VSource objects currently defined in the active circuit."""
        return self.dss_obj.VsourcesI(ctypes.c_int32(0), ctypes.c_int32(0))

    def vsources_first(self) -> int:
        """Sets the first VSource to be active; returns 0 if none."""
        return self.dss_obj.VsourcesI(ctypes.c_int32(1), ctypes.c_int32(0))

    def vsources_next(self) -> int:
        """Sets the next VSource to be active; returns 0 if none."""
        return self.dss_obj.VsourcesI(ctypes.c_int32(2), ctypes.c_int32(0))

    def vsources_read_phases(self) -> int:
        """Gets the number of phases of the active VSource."""
        return self.dss_obj.VsourcesI(ctypes.c_int32(3), ctypes.c_int32(0))

    def vsources_write_phases(self, argument) -> int:
        """Sets the number of phases of the active VSource."""
        argument = Base.check_int_param(argument)
        return self.dss_obj.VsourcesI(ctypes.c_int32(4), ctypes.c_int32(argument))
