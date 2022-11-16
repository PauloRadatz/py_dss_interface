# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Lines.LinesF import LinesF
from py_dss_interface.models.Lines.LinesI import LinesI
from py_dss_interface.models.Lines.LinesS import LinesS
from py_dss_interface.models.Lines.LinesV import LinesV
from typing import List


class Lines(LinesV, LinesS, LinesI, LinesF):
    """
    This interface implements the Lines (ILines) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: LinesV, LinesS, LinesI, LinesF.
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def length(self) -> float:
        return LinesF._length_read(self)

    @length.setter
    def length(self, arg: float):
        LinesF._length_write(self, arg)

    @property
    def r1(self) -> float:
        return LinesF._r1_read(self)

    @r1.setter
    def r1(self, arg: float):
        LinesF._r1_write(self, arg)

    @property
    def x1(self) -> float:
        return LinesF._x1_read(self)

    @x1.setter
    def x1(self, arg: float):
        LinesF._x1_write(self, arg)

    @property
    def r0(self) -> float:
        return LinesF._r0_read(self)

    @r0.setter
    def r0(self, arg: float):
        LinesF._r0_write(self, arg)

    @property
    def x0(self) -> float:
        return LinesF._x0_read(self)

    @x0.setter
    def x0(self, arg: float):
        LinesF._x0_write(self, arg)

    @property
    def c1(self) -> float:
        return LinesF._c1_read(self)

    @c1.setter
    def c1(self, arg: float):
        LinesF._c1_write(self, arg)

    @property
    def c0(self) -> float:
        return LinesF._c0_read(self)

    @c0.setter
    def c0(self, arg: float):
        LinesF._c0_write(self, arg)

    @property
    def norm_amps(self) -> float:
        return LinesF._norm_amps_read(self)

    @norm_amps.setter
    def norm_amps(self, arg: float):
        LinesF._norm_amps_write(self, arg)

    @property
    def emerg_amps(self) -> float:
        return LinesF._emerg_amps_read(self)

    @emerg_amps.setter
    def emerg_amps(self, arg: float):
        LinesF._emerg_amps_write(self, arg)

    @property
    def rg(self) -> float:
        return LinesF._rg_read(self)

    @rg.setter
    def rg(self, arg: float):
        LinesF._rg_write(self, arg)

    @property
    def xg(self) -> float:
        return LinesF._xg_read(self)

    @xg.setter
    def xg(self, arg: float):
        LinesF._xg_write(self, arg)

    @property
    def rho(self) -> float:
        return LinesF._rho_read(self)

    @rho.setter
    def rho(self, arg: float):
        LinesF._rho_write(self, arg)

    @property
    def season_rating(self) -> float:
        return LinesF._season_rating_read(self)

    @property
    def count(self) -> int:
        return LinesI._count(self)

    def first(self) -> int:
        return LinesI._first(self)

    def next(self) -> int:
        return LinesI._next(self)

    @property
    def units(self) -> int:
        return LinesI._units_read(self)

    @units.setter
    def units(self, arg: int):
        LinesI._units_write(self, arg)

    @property
    def phases(self) -> int:
        return LinesI._phases_read(self)

    @phases.setter
    def phases(self, arg: int):
        LinesI._phases_write(self, arg)

    @property
    def num_cust(self) -> int:
        return LinesI._num_cust(self)

    @property
    def parent(self) -> int:
        return LinesI._parent(self)

    @property
    def name(self) -> str:
        return LinesS._name_read(self)

    @name.setter
    def name(self, arg: str):
        LinesS._name_write(self, arg)

    @property
    def bus1(self) -> str:
        return LinesS._bus1_read(self)

    @bus1.setter
    def bus1(self, arg: str):
        LinesS._bus1_write(self, arg)

    @property
    def bus2(self) -> str:
        return LinesS._bus2_read(self)

    @bus2.setter
    def bus2(self, arg: str):
        LinesS._bus2_write(self, arg)

    @property
    def linecode(self) -> str:
        return LinesS._linecode_read(self)

    @linecode.setter
    def linecode(self, arg: str):
        LinesS._linecode_write(self, arg)

    @property
    def geometry(self) -> str:
        return LinesS._geometry_read(self)

    @geometry.setter
    def geometry(self, arg: str):
        LinesS._geometry_write(self, arg)

    @property
    def spacing(self) -> str:
        return LinesS._spacing_read(self)

    @spacing.setter
    def spacing(self, arg: str):
        LinesS._spacing_write(self, arg)

    @property
    def rmatrix(self) -> str:
        return LinesV._rmatrix_read(self)

    @rmatrix.setter
    def rmatrix(self, arg: str):
        LinesV._rmatrix_write(self, arg)

    @property
    def xmatrix(self) -> List[float]:
        return LinesV._xmatrix_read(self)

    @xmatrix.setter
    def xmatrix(self, arg: str):
        LinesV._xmatrix_write(self, arg)

    @property
    def cmatrix(self) -> str:
        return LinesV._cmatrix_read(self)

    @cmatrix.setter
    def cmatrix(self, arg: str):
        LinesV._cmatrix_write(self, arg)

    @property
    def names(self) -> str:
        return LinesV._names(self)

    @property
    def yprim(self) -> str:
        return LinesV._yprim_read(self)

    @yprim.setter
    def yprim(self, arg: str):
        LinesV._yprim_write(self, arg)




