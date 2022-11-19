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
        return LoadShapesI._count(self)

    def first(self) -> int:
        return LoadShapesI._first(self)

    @property
    def hr_interval(self) -> float:
        return LoadShapesF._hr_interval(self)

    @hr_interval.setter
    def hr_interval(self, arg: float):
        LoadShapesF._hr_interval_write(self, arg)

    @property
    def min_interval(self) -> float:
        return LoadShapesF._min_interval(self)

    @min_interval.setter
    def min_interval(self, arg: float):
        LoadShapesF._min_interval_write(self, arg)

    @property
    def name(self) -> str:
        return LoadShapesS._name(self)

    @name.setter
    def name(self, arg: str):
        LoadShapesS._name_write(self, arg)

    @property
    def names(self) -> List[str]:
        return LoadShapesV._names(self)

    def next(self) -> int:
        return LoadShapesI._next(self)

    def normalize(self) -> int:
        return LoadShapesI._normalize(self)

    @property
    def npts(self) -> int:
        return LoadShapesI._npts(self)

    @npts.setter
    def npts(self, arg: int):
        LoadShapesI._npts_write(self, arg)

    @property
    def p_base(self) -> float:
        return LoadShapesF._p_base(self)

    @p_base.setter
    def p_base(self, arg: float):
        LoadShapesF._p_base_write(self, arg)

    @property
    def p_mult(self) -> List[float]:
        return LoadShapesV._p_mult(self)

    @p_mult.setter
    def p_mult(self, arg: List[float]):
        LoadShapesV._p_mult_write(self, arg)

    @property
    def q_base(self) -> float:
        return LoadShapesF._q_base(self)

    @q_base.setter
    def q_base(self, arg: float):
        LoadShapesF._q_base_write(self, arg)

    @property
    def q_mult(self) -> List[float]:
        return LoadShapesV._q_mult(self)

    @q_mult.setter
    def q_mult(self, arg: List[float]):
        LoadShapesV._q_mult_write(self, arg)

    @property
    def s_interval(self) -> float:
        return LoadShapesF._s_interval(self)

    @s_interval.setter
    def s_interval(self, arg: float):
        LoadShapesF._s_interval_write(self, arg)

    @property
    def time_array(self) -> List[float]:
        return LoadShapesV._time_array(self)

    @time_array.setter
    def time_array(self, arg: List[float]):
        LoadShapesV._time_array_write(self, arg)

    @property
    def use_actual(self) -> int:
        return LoadShapesI._use_actual(self)

    @use_actual.setter
    def use_actual(self, arg: int):
        LoadShapesI._use_actual_write(self, arg)
