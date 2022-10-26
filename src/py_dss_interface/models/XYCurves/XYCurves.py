# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.XYCurves.XYCurvesF import XYCurvesF
from py_dss_interface.models.XYCurves.XYCurvesI import XYCurvesI
from py_dss_interface.models.XYCurves.XYCurvesS import XYCurvesS
from py_dss_interface.models.XYCurves.XYCurvesV import XYCurvesV


class XYCurves(XYCurvesS, XYCurvesI, XYCurvesF, XYCurvesV):
    """
    This interface implements the XYCurves (IXYCurves) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: XYCurvesS, XYCurvesI, XYCurvesF, XYCurvesV.
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def x(self) -> float:
        return XYCurvesF._x(self)

    @x.setter
    def x(self, value: int):
        XYCurvesF._x_write(self, value)

