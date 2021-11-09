# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class LoadShapesI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t LoadShapeI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def loadshapes_count(self) -> int:
        """Returns the number of LoadShape objects currently defined in LoadShape collection."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(0), ctypes.c_int32(0))

    # TODO include in test
    def loadshapes_first(self) -> int:
        """sets the first loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(1), ctypes.c_int32(0))

    def loadshapes_next(self) -> int:
        """Sets the next loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(2), ctypes.c_int32(0))

    def loadshapes_read_npts(self) -> int:
        """Gets the number of points in active LoadShape."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(3), ctypes.c_int32(0))

    def loadshapes_write_npts(self, argument) -> int:
        """Sets the number of points in active LoadShape."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def loadshapes_normalize(self) -> int:
        """Normalizes the P and Q curves based on either Pbase, Qbase or simply the peak value of the curve."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(5), ctypes.c_int32(0))

    def loadshapes_read_use_actual(self) -> int:
        """Gets a TRUE/FALSE (1/0) to let Loads know to use the actual value in the curve rather than use the value as
         a multiplier."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(6), ctypes.c_int32(0))

    def loadshapes_write_use_actual(self, argument) -> int:
        """Sets a TRUE/FALSE (1/0 - Argument) to let Loads know to use the actual value in the curve rather than use
         the value as a multiplier."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(7), ctypes.c_int32(argument))
