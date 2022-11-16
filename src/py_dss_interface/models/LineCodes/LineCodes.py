# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.LineCodes.LineCodesF import LineCodesF
from py_dss_interface.models.LineCodes.LineCodesI import LineCodesI
from py_dss_interface.models.LineCodes.LineCodesS import LineCodesS
from py_dss_interface.models.LineCodes.LineCodesV import LineCodesV


class LineCodes(LineCodesF, LineCodesS, LineCodesI, LineCodesV):
    """
    This interface implements the Lines (ILineCodes) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface:
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def r1(self) -> float:
        return LineCodesF._r1_read(self)

    @r1.setter
    def r1(self, arg: float):
        LineCodesF._r1_write(self, arg)

    @property
    def x1(self) -> float:
        return LineCodesF._x1_read(self)

    @x1.setter
    def x1(self, arg: float):
        LineCodesF._x1_write(self, arg)

    @property
    def r0(self) -> float:
        return LineCodesF._r0_read(self)

    @r0.setter
    def r0(self, arg: float):
        LineCodesF._r0_write(self, arg)

    @property
    def x0(self) -> float:
        return LineCodesF._x0_read(self)

    @x0.setter
    def x0(self, arg: float):
        LineCodesF._x0_write(self, arg)

    @property
    def c1(self) -> float:
        return LineCodesF._c1_read(self)

    @c1.setter
    def c1(self, arg: float):
        LineCodesF._c1_write(self, arg)

    @property
    def c0(self) -> float:
        return LineCodesF._c0_read(self)

    @c0.setter
    def c0(self, arg: float):
        LineCodesF._c0_write(self, arg)

    @property
    def norm_amps(self) -> float:
        return LineCodesF._norm_amps_read(self)

    @norm_amps.setter
    def norm_amps(self, arg: float):
        LineCodesF._norm_amps_write(self, arg)

    @property
    def emerg_amps(self) -> float:
        return LineCodesF._emerg_amps_read(self)

    @emerg_amps.setter
    def emerg_amps(self, arg: float):
        LineCodesF._emerg_amps_write(self, arg)

    @property
    def count(self) -> int:
        return LineCodesI._count(self)

    def first(self) -> int:
        return LineCodesI._first(self)

    def next(self) -> int:
        return LineCodesI._next(self)

    @property
    def units(self) -> int:
        return LineCodesI._units_read(self)

    @units.setter
    def units(self, arg: int):
        LineCodesI._units_write(self, arg)

    @property
    def phases(self) -> int:
        return LineCodesI._phases_read(self)

    @phases.setter
    def phases(self, arg: int):
        LineCodesI._phases_write(self, arg)

    @property
    def is_z1z0(self) -> int:
        return LineCodesI._is_z1z0(self)

    @property
    def name(self) -> str:
        return LineCodesS._name_read(self)

    @name.setter
    def name(self, arg: str):
        LineCodesS._name_write(self, arg)

    @property
    def rmatrix(self) -> str:
        return LineCodesV._rmatrix_read(self)

    @rmatrix.setter
    def rmatrix(self, arg: str):
        LineCodesV._rmatrix_write(self, arg)

    @property
    def xmatrix(self) -> str:
        return LineCodesV._xmatrix_read(self)

    @xmatrix.setter
    def xmatrix(self, arg: str):
        LineCodesV._xmatrix_write(self, arg)

    @property
    def cmatrix(self) -> str:
        return LineCodesV._cmatrix_read(self)

    @cmatrix.setter
    def cmatrix(self, arg: str):
        LineCodesV._cmatrix_write(self, arg)

    @property
    def names(self) -> str:
        return LineCodesV._names(self)
