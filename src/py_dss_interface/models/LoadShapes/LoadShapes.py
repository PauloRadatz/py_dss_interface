# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.LoadShapes.LoadShapesF import LoadShapesF
from py_dss_interface.models.LoadShapes.LoadShapesI import LoadShapesI
from py_dss_interface.models.LoadShapes.LoadShapesS import LoadShapesS
from py_dss_interface.models.LoadShapes.LoadShapesV import LoadShapesV
from typing import List


class LoadShapes(LoadShapesF, LoadShapesI, LoadShapesS, LoadShapesV):
    """
    This interface implements the LoadShape (ILoadShape) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def count(self) -> int:
        """Returns the number of LoadShape objects currently _defined in LoadShape collection."""
        return LoadShapesI._count(self)

    def first(self) -> int:
        """sets the first loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        return LoadShapesI._first(self)

    @property
    def hr_interval(self) -> float:
        """Gets the fixed interval time value, hours.
        Sets the fixed interval time value, hours."""
        return LoadShapesF._hr_interval(self)

    @hr_interval.setter
    def hr_interval(self, arg: float):
        LoadShapesF._hr_interval_write(self, arg)

    @property
    def min_interval(self) -> float:
        """Gets the fixed interval time value, in minutes.
        Sets the fixed interval time value, in minutes."""
        return LoadShapesF._min_interval(self)

    @min_interval.setter
    def min_interval(self, arg: float):
        LoadShapesF._min_interval_write(self, arg)

    @property
    def name(self) -> str:
        """Gets the name of the active LoadShape object.
        Sets the name of the active LoadShape object."""
        return LoadShapesS._name(self)

    @name.setter
    def name(self, arg: str):
        LoadShapesS._name_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings containing names of all LoadShape objects currently defined."""
        return LoadShapesV._names(self)

    def next(self) -> int:
        """Sets the next loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        return LoadShapesI._next(self)

    def normalize(self) -> int:
        """Normalizes the P and Q curves based on either Pbase, Qbase or simply the peak value of the curve."""
        return LoadShapesI._normalize(self)

    @property
    def npts(self) -> int:
        """Gets the number of points in active LoadShape.
        Sets the number of points in active LoadShape."""
        return LoadShapesI._npts(self)

    @npts.setter
    def npts(self, arg: int):
        LoadShapesI._npts_write(self, arg)

    @property
    def p_base(self) -> float:
        """Gets the base for normalizing P curve. If left at zero, the peak value is used.
        Sets the base for normalizing P curve. If left at zero, the peak value is used."""
        return LoadShapesF._p_base(self)

    @p_base.setter
    def p_base(self, arg: float):
        LoadShapesF._p_base_write(self, arg)

    @property
    def p_mult(self) -> List[float]:
        """Gets a variant array of doubles for the P multiplier in the LoadShape.
        Sets a variant array of doubles for the P multiplier in the LoadShape."""
        return LoadShapesV._p_mult(self)

    @p_mult.setter
    def p_mult(self, arg: List[float]):
        LoadShapesV._p_mult_write(self, arg)

    @property
    def q_base(self) -> float:
        """Gets the base for normalizing Q curve. If left at zero, the peak value is used.
        Sets the base for normalizing Q curve. If left at zero, the peak value is used."""
        return LoadShapesF._q_base(self)

    @q_base.setter
    def q_base(self, arg: float):
        LoadShapesF._q_base_write(self, arg)

    @property
    def q_mult(self) -> List[float]:
        """Gets a variant array of doubles for the Q multiplier in the LoadShape.
        Sets a variant array of doubles for the Q multiplier in the LoadShape."""
        return LoadShapesV._q_mult(self)

    @q_mult.setter
    def q_mult(self, arg: List[float]):
        LoadShapesV._q_mult_write(self, arg)

    @property
    def s_interval(self) -> float:
        """Gets the fixed interval data time interval, seconds.
        Sets the fixed interval data time interval, seconds."""
        return LoadShapesF._s_interval(self)

    @s_interval.setter
    def s_interval(self, arg: float):
        LoadShapesF._s_interval_write(self, arg)

    @property
    def time_array(self) -> List[float]:
        """Gets a time array in hours corresponding to P and Q multipliers when the Interval = 0.
        Sets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        return LoadShapesV._time_array(self)

    @time_array.setter
    def time_array(self, arg: List[float]):
        LoadShapesV._time_array_write(self, arg)

    @property
    def use_actual(self) -> int:
        """Gets a TRUE/FALSE (1/0) to let Loads know to use the actual value in the curve rather than use the value as
                 a multiplier.
        Sets a TRUE/FALSE (1/0 - Argument) to let Loads know to use the actual value in the curve rather than use
         the value as a multiplier."""
        return LoadShapesI._use_actual(self)

    @use_actual.setter
    def use_actual(self, arg: int):
        LoadShapesI._use_actual_write(self, arg)
