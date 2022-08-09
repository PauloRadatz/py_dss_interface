# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.LoadShapes.LoadShapesF import LoadShapesF
from py_dss_interface.models.LoadShapes.LoadShapesI import LoadShapesI
from py_dss_interface.models.LoadShapes.LoadShapesS import LoadShapesS
from py_dss_interface.models.LoadShapes.LoadShapesV import LoadShapesV


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

    @property
    def first(self) -> int:
        return LoadShapesI._first(self)

    @property
    def hr_interval(self) -> float:
        return LoadShapesF._hr_interval(self)

    @hr_interval.setter
    def hr_interval(self, value: float):
        LoadShapesF._hr_interval_write(self, value)

    @property
    def min_interval(self) -> float:
        return LoadShapesF._min_interval(self)

    @min_interval.setter
    def min_interval(self, value: float):
        LoadShapesF._min_interval_write(self, value)

    @property
    def name(self) -> str:
        return LoadShapesS._name(self)

    @name.setter
    def name(self, value: str):
        LoadShapesS._name_write(self, value)

    @property
    def names(self):
        return LoadShapesV._names(self)

    @property
    def next(self) -> int:
        return LoadShapesI._next(self)

    @property
    def normalize(self) -> int:
        return LoadShapesI._normalize(self)

    @property
    def npts(self) -> int:
        return LoadShapesI._npts(self)

    @npts.setter
    def npts(self, value: int):
        LoadShapesI._npts_write(self, value)

    @property
    def p_base(self) -> float:
        return LoadShapesF._p_base(self)

    @p_base.setter
    def p_base(self, value: float):
        LoadShapesF._p_base_write(self, value)

    @property
    def p_mult(self):
        return LoadShapesV._p_mult(self)

    @p_mult.setter
    def p_mult(self, value):
        LoadShapesV._p_mult_write(self, value)

    @property
    def q_base(self) -> float:
        return LoadShapesF._q_base(self)

    @q_base.setter
    def q_base(self, value: float):
        LoadShapesF._q_base_write(self, value)

    @property
    def q_mult(self):
        return LoadShapesV._q_mult(self)

    @q_mult.setter
    def q_mult(self, value):
        LoadShapesV._q_mult_write(self, value)

    @property
    def s_interval(self) -> float:
        return LoadShapesF._s_interval(self)

    @s_interval.setter
    def s_interval(self, value: float):
        LoadShapesF._s_interval_write(self, value)

    @property
    def time_array(self):
        return LoadShapesV._time_array(self)

    @time_array.setter
    def time_array(self, value):
        LoadShapesV._time_array_write(self, value)

    @property
    def use_actual(self) -> int:
        return LoadShapesI._use_actual(self)

    @use_actual.setter
    def use_actual(self, value: int):
        LoadShapesI._use_actual_write(self, value)
