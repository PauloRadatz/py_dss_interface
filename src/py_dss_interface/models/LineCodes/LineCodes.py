# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.LineCodes.LineCodesF import LineCodesF
from py_dss_interface.models.LineCodes.LineCodesI import LineCodesI
from py_dss_interface.models.LineCodes.LineCodesS import LineCodesS
from py_dss_interface.models.LineCodes.LineCodesV import LineCodesV
from typing import List

class LineCodes(LineCodesF, LineCodesS, LineCodesI, LineCodesV):
    """
    This interface implements the Lines (ILineCodes) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface:
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def r1(self) -> float:
        """Gets the Positive-sequence resistance in ohms per unit length for the active LineCode.

        Sets the Positive-sequence resistance in ohms per unit length for the active LineCode.
                This value must be specified in the argument as a double."""
        return LineCodesF._r1_read(self)

    @r1.setter
    def r1(self, arg: float):
        LineCodesF._r1_write(self, arg)

    @property
    def x1(self) -> float:
        """Gets the Positive-sequence reactance in ohms per unit length for the active LineCode.

        Sets the Positive-sequence reactance in ohms per unit length for the active LineCode.
                This value must be specified in the argument as a double."""
        return LineCodesF._x1_read(self)

    @x1.setter
    def x1(self, arg: float):
        LineCodesF._x1_write(self, arg)

    @property
    def r0(self) -> float:
        """Gets the Zero-sequence resistance in ohms per unit length for the active LineCode.

        Sets the Zero-sequence resistance in ohms per unit length for the active LineCode.
                This value must be specified in the argument as a double."""
        return LineCodesF._r0_read(self)

    @r0.setter
    def r0(self, arg: float):
        LineCodesF._r0_write(self, arg)

    @property
    def x0(self) -> float:
        """Gets the Zero-sequence reactance in ohms per unit length for the active LineCode.

        Sets the Zero-sequence reactance in ohms per unit length for the active LineCode.
                This value must be specified in the argument as a double."""
        return LineCodesF._x0_read(self)

    @x0.setter
    def x0(self, arg: float):
        LineCodesF._x0_write(self, arg)

    @property
    def c1(self) -> float:
        """Gets the Positive-sequence capacitance in nF per unit length for the active LineCode.

        Sets the Positive-sequence capacitance in nF per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        return LineCodesF._c1_read(self)

    @c1.setter
    def c1(self, arg: float):
        LineCodesF._c1_write(self, arg)

    @property
    def c0(self) -> float:
        """Gets the Zero-sequence capacitance in ohms per unit length for the active LineCode.

        Sets the Zero-sequence capacitance in ohms per unit length for the active LineCode.
                This value must be specified in the argument as a double."""
        return LineCodesF._c0_read(self)

    @c0.setter
    def c0(self, arg: float):
        LineCodesF._c0_write(self, arg)

    @property
    def norm_amps(self) -> float:
        """Gets the normal ampere rating for the active LineCode.

        Sets the normal ampere rating for the active LineCode. This value must be specified in the argument
                as a double."""
        return LineCodesF._norm_amps_read(self)

    @norm_amps.setter
    def norm_amps(self, arg: float):
        LineCodesF._norm_amps_write(self, arg)

    @property
    def emerg_amps(self) -> float:
        """Gets the Emergency ampere rating for the active LineCode.

        Sets the Emergency ampere rating for the active LineCode. This value must be specified in the argument
                as a double."""
        return LineCodesF._emerg_amps_read(self)

    @emerg_amps.setter
    def emerg_amps(self, arg: float):
        LineCodesF._emerg_amps_write(self, arg)

    @property
    def count(self) -> int:
        """Gets the number of Line Objects in Active Circuit."""
        return LineCodesI._count(self)

    def first(self) -> int:
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return LineCodesI._first(self)

    def next(self) -> int:
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        return LineCodesI._next(self)

    @property
    def units(self) -> int:
        """Delivers the units of the active LineCode as an integer.

        Sets the units of the active LineCode. The units must be specified as an integer in the argument.
                Please refer to the OpenDSS User manual for more information.
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
        return LineCodesI._units_read(self)

    @units.setter
    def units(self, arg: int):
        LineCodesI._units_write(self, arg)

    @property
    def phases(self) -> int:
        """Delivers the number of phases of the active LineCode as an integer.

        Sets the number of phases of the active LineCode. The units must be specified as an integer in the
                argument. """
        return LineCodesI._phases_read(self)

    @phases.setter
    def phases(self, arg: int):
        LineCodesI._phases_write(self, arg)

    @property
    def is_z1z0(self) -> int:
        """Gets the flag (Boolean 1/0) denoting whether the impedance data were entered in symmetrical components."""
        return LineCodesI._is_z1z0(self)

    @property
    def name(self) -> str:
        """Gets the name of the active LineCode element.

        Sets the name of the active LineCode element. The new value must be specified in the argument as a string."""
        return LineCodesS._name_read(self)

    @name.setter
    def name(self, arg: str):
        LineCodesS._name_write(self, arg)

    @property
    def rmatrix(self) -> List[float]:
        """Gets the resistance matrix in ohms per unit length of the active LineCode.

        Sets the resistance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument.
         """
        return LineCodesV._rmatrix_read(self)

    @rmatrix.setter
    def rmatrix(self, arg: List[float]):
        LineCodesV._rmatrix_write(self, arg)

    @property
    def xmatrix(self) -> List[float]:
        """Gets the reactance matrix in ohms per unit length of the active LineCode.

        Sets the reactance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument.
         """
        return LineCodesV._xmatrix_read(self)

    @xmatrix.setter
    def xmatrix(self, arg: List[float]):
        LineCodesV._xmatrix_write(self, arg)

    @property
    def cmatrix(self) -> List[float]:
        """Gets the capacitance matrix in ohms per unit length of the active LineCode.

        Sets the capacitance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument.
         """
        return LineCodesV._cmatrix_read(self)

    @cmatrix.setter
    def cmatrix(self, arg: List[float]):
        LineCodesV._cmatrix_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        return LineCodesV._names(self)
