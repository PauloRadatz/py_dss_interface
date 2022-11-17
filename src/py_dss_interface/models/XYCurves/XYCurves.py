# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.XYCurves.XYCurvesF import XYCurvesF
from py_dss_interface.models.XYCurves.XYCurvesI import XYCurvesI
from py_dss_interface.models.XYCurves.XYCurvesS import XYCurvesS
from py_dss_interface.models.XYCurves.XYCurvesV import XYCurvesV
from typing import List


class XYCurves(XYCurvesS, XYCurvesI, XYCurvesF, XYCurvesV):
    """
    This interface implements the XYCurves (IXYCurves) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: XYCurvesS, XYCurvesI, XYCurvesF, XYCurvesV.
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def x(self) -> float:
        return XYCurvesF._x_read(self)

    @x.setter
    def x(self, value: int):
        XYCurvesF._x_write(self, value)

    @property
    def y(self) -> float:
        return XYCurvesF._y_read(self)

    @y.setter
    def y(self, value: int):
        XYCurvesF._y_write(self, value)

    @property
    def x_shift(self) -> float:
        return XYCurvesF._x_shift_read(self)

    @x_shift.setter
    def x_shift(self, value: int):
        XYCurvesF._x_shift_write(self, value)

    @property
    def y_shift(self) -> float:
        return XYCurvesF._y_shift_read(self)

    @y_shift.setter
    def y_shift(self, value: int):
        XYCurvesF._y_shift_write(self, value)

    @property
    def x_scale(self) -> float:
        return XYCurvesF._x_scale_read(self)

    @x_scale.setter
    def x_scale(self, value: int):
        XYCurvesF._x_scale_write(self, value)

    @property
    def y_scale(self) -> float:
        return XYCurvesF._y_scale_read(self)

    @y_scale.setter
    def y_scale(self, value: int):
        XYCurvesF._y_scale_write(self, value)

    @property
    def count(self) -> int:
        return XYCurvesI._count(self)

    def first(self) -> int:
        return XYCurvesI._first(self)

    def next(self) -> int:
        return XYCurvesI._next(self)

    @property
    def npts(self) -> int:
        return XYCurvesI._npts_read(self)

    @npts.setter
    def npts(self, argument: int):
        XYCurvesI._npts_write(self, argument)

    @property
    def name(self) -> str:
        return XYCurvesS._name_read(self)

    @name.setter
    def name(self, argument: str):
        XYCurvesS._name_write(self, argument)

    @property
    def x_array(self) -> List[float]:
        return XYCurvesV._x_array_read(self)

    @x_array.setter
    def x_array(self, argument: List[float]):
        XYCurvesV._x_array_write(self, argument)

    @property
    def y_array(self) -> List[float]:
        return XYCurvesV._y_array_read(self)

    @y_array.setter
    def y_array(self, argument: List[float]):
        XYCurvesV._y_array_write(self, argument)
