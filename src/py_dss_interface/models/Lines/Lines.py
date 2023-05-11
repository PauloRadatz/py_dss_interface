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
        """Gets the length of line section in units compatible with the LineCode definition.

        Sets the length of line section in units compatible with the LineCode definition."""
        return LinesF._length_read(self)

    @length.setter
    def length(self, arg: float):
        LinesF._length_write(self, arg)

    @property
    def r1(self) -> float:
        """Gets the positive sequence resistance, ohm per unit length.

        Sets the positive sequence resistance, ohm per unit length."""
        return LinesF._r1_read(self)

    @r1.setter
    def r1(self, arg: float):
        LinesF._r1_write(self, arg)

    @property
    def x1(self) -> float:
        """Gets the positive sequence reactance, ohm per unit length.

        Sets the positive sequence reactance, ohm per unit length."""
        return LinesF._x1_read(self)

    @x1.setter
    def x1(self, arg: float):
        LinesF._x1_write(self, arg)

    @property
    def r0(self) -> float:
        """Gets the zero sequence resistance, ohm per unit length.

        Sets the zero sequence resistance, ohm per unit length."""
        return LinesF._r0_read(self)

    @r0.setter
    def r0(self, arg: float):
        LinesF._r0_write(self, arg)

    @property
    def x0(self) -> float:
        """Gets the zero sequence reactance, ohm per unit length.

        Sets the zero sequence reactance, ohm per unit length."""
        return LinesF._x0_read(self)

    @x0.setter
    def x0(self, arg: float):
        LinesF._x0_write(self, arg)

    @property
    def c1(self) -> float:
        """Gets the positive sequence capacitance, nanofarads per unit length.

        Sets the positive sequence capacitance, nanofarads per unit length."""
        return LinesF._c1_read(self)

    @c1.setter
    def c1(self, arg: float):
        LinesF._c1_write(self, arg)

    @property
    def c0(self) -> float:
        """Gets the zero sequence capacitance, nanofarads per unit length.

        Sets the zero sequence capacitance, nanofarads per unit length."""
        return LinesF._c0_read(self)

    @c0.setter
    def c0(self, arg: float):
        LinesF._c0_write(self, arg)

    @property
    def norm_amps(self) -> float:
        """Gets the normal ampere rating of line section.

        Sets the normal ampere rating of Line."""
        return LinesF._norm_amps_read(self)

    @norm_amps.setter
    def norm_amps(self, arg: float):
        LinesF._norm_amps_write(self, arg)

    @property
    def emerg_amps(self) -> float:
        """Gets the emergency (maximum) ampere rating of Line.

        Sets the emergency (maximum) ampere rating of Line."""
        return LinesF._emerg_amps_read(self)

    @emerg_amps.setter
    def emerg_amps(self, arg: float):
        LinesF._emerg_amps_write(self, arg)

    @property
    def rg(self) -> float:
        """Gets the earth return value used to compute line impedance's at power frequency.

        Sets the earth return value used to compute line impedances at power frequency."""
        return LinesF._rg_read(self)

    @rg.setter
    def rg(self, arg: float):
        LinesF._rg_write(self, arg)

    @property
    def xg(self) -> float:
        """Gets the earth return reactance value used to compute line impedances at power frequency.

        Sets the earth return reactance value used to compute line impedances at power frequency."""
        return LinesF._xg_read(self)

    @xg.setter
    def xg(self, arg: float):
        LinesF._xg_write(self, arg)

    @property
    def rho(self) -> float:
        """Gets the earth resistivity, m-ohms.

        Sets the earth resistivity, m-ohms."""
        return LinesF._rho_read(self)

    @rho.setter
    def rho(self, arg: float):
        LinesF._rho_write(self, arg)

    @property
    def season_rating(self) -> float:
        """Returns the rating for the current season (in Amps) if the SeasonalRatings option is active."""
        return LinesF._season_rating_read(self)

    @property
    def count(self) -> int:
        """Gets the number of Line Objects in Active Circuit."""
        return LinesI._count(self)

    def first(self) -> int:
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return LinesI._first(self)

    def next(self) -> int:
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return LinesI._next(self)

    @property
    def units(self) -> int:
        """Gets the units of the line (distance, check manual for details).

        Sets the units of the line (distance, check manual for details).
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
        return LinesI._units_read(self)

    @units.setter
    def units(self, arg: int):
        LinesI._units_write(self, arg)

    @property
    def phases(self) -> int:
        """Gets the number of phases of the active line object.

        Sets the number of phases of the active line object."""
        return LinesI._phases_read(self)

    @phases.setter
    def phases(self, arg: int):
        LinesI._phases_write(self, arg)

    @property
    def num_cust(self) -> int:
        """Gets the number of customers on this line section."""
        return LinesI._num_cust(self)

    @property
    def parent(self) -> int:
        """Gets the parents of the active Line to be the active Line. Return 0 if no parent or action fails."""
        return LinesI._parent(self)

    @property
    def name(self) -> str:
        """Gets the name of the active Line element.
        Sets the name of the Line element to set it active."""
        return LinesS._name_read(self)

    @name.setter
    def name(self, arg: str):
        LinesS._name_write(self, arg)

    @property
    def bus1(self) -> str:
        """Gets the name of bus for terminal 1.

        Sets the name of bus for terminal 1."""
        return LinesS._bus1_read(self)

    @bus1.setter
    def bus1(self, arg: str):
        LinesS._bus1_write(self, arg)

    @property
    def bus2(self) -> str:
        """Gets the name of bus for terminal 2.

        Sets the name of bus for terminal 2."""
        return LinesS._bus2_read(self)

    @bus2.setter
    def bus2(self, arg: str):
        LinesS._bus2_write(self, arg)

    @property
    def linecode(self) -> str:
        """Gets the name of LineCode object that defines the impedances.

        Sets the name of LineCode object that defines the impedances."""
        return LinesS._linecode_read(self)

    @linecode.setter
    def linecode(self, arg: str):
        LinesS._linecode_write(self, arg)

    @property
    def geometry(self) -> str:
        """Gets the name of the Line geometry code.

        Sets the name of the Line geometry code."""
        return LinesS._geometry_read(self)

    @geometry.setter
    def geometry(self, arg: str):
        LinesS._geometry_write(self, arg)

    @property
    def spacing(self) -> str:
        """Gets the name of the Line spacing code.

        Sets the name of the Line spacing code."""
        return LinesS._spacing_read(self)

    @spacing.setter
    def spacing(self, arg: str):
        LinesS._spacing_write(self, arg)

    @property
    def rmatrix(self) -> List[float]:
        """Gets the resistance matrix (full), ohms per unit length. Variant array of doubles.

        Sets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        return LinesV._rmatrix_read(self)

    @rmatrix.setter
    def rmatrix(self, arg: List[float]):
        LinesV._rmatrix_write(self, arg)

    @property
    def xmatrix(self) -> List[float]:
        """Gets the reactance matrix (full), ohms per unit length. Variant array of doubles.

        Sets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        return LinesV._xmatrix_read(self)

    @xmatrix.setter
    def xmatrix(self, arg: List[float]):
        LinesV._xmatrix_write(self, arg)

    @property
    def cmatrix(self) -> List[float]:
        """Gets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles.

        Sets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        return LinesV._cmatrix_read(self)

    @cmatrix.setter
    def cmatrix(self, arg: List[float]):
        LinesV._cmatrix_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Gets the name of all Line Objects."""
        return LinesV._names(self)

    @property
    def yprim(self) -> List[float]:
        """Gets the YPrimitive of the active Line.

        According to the official documentation this parameter does nothing at present."""
        return LinesV._yprim_read(self)

    @yprim.setter
    def yprim(self, arg: List[float]):
        LinesV._yprim_write(self, arg)




