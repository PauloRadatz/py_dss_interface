# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 8:27 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : Reactors.py
# @Software: PyCharm

# -*- coding: iso-8859-15 -*-

from py_dss_interface.models.Reactors.ReactorsF import ReactorsF
from py_dss_interface.models.Reactors.ReactorsI import ReactorsI
from py_dss_interface.models.Reactors.ReactorsS import ReactorsS
from py_dss_interface.models.Reactors.ReactorsV import ReactorsV
from typing import List


class Reactors(ReactorsI, ReactorsV, ReactorsS, ReactorsF):
    """
    This interface implements the Reactors (IReactors) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ReactorsI, ReactorsV, ReactorsS, ReactorsF._
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def first(self) -> int:
        """Set first Reactors element active; returns 0 if none."""
        return ReactorsI._first(self)

    def next(self) -> int:
        """Sets next Reactors element active; returns 0 if no more."""
        return ReactorsI._next(self)

    @property
    def count(self) -> int:
        """Returns the number of Reactors Elements."""
        return ReactorsI._count(self)

    @property
    def parallel(self) -> int:
        """This parameter gets the flag (1/0) indicating whether Rmatrix and Xmatrix are to be considered in parallel for the active reactor.

        This parameter sets the flag (1/0) indicating whether Rmatrix and Xmatrix are to be considered in parallel for the active reactor."""
        return ReactorsI._parallel(self)

    @parallel.setter
    def parallel(self, arg: int):
        ReactorsI._parallel_write(self, arg)

    @property
    def kv(self) -> float:
        """Gets/Sets the kV rating of the active reactor."""
        return ReactorsF._kv(self)

    @kv.setter
    def kv(self, arg: float):
        ReactorsF._kv_write(self, arg)

    @property
    def kvar(self) -> float:
        """Gets/Sets the kvar rating of the active reactor."""
        return ReactorsF._kvar(self)

    @kvar.setter
    def kvar(self, arg: float):
        ReactorsF._kvar_write(self, arg)

    @property
    def imh(self) -> float:
        """Gets/Sets the Inductance, mH for the active reactor."""
        return ReactorsF._imh(self)

    @imh.setter
    def imh(self, arg: float):
        ReactorsF._imh_write(self, arg)

    @property
    def r(self) -> float:
        """Gets/Sets the Resistance (in series with reactance), each phase, ohms for the active reactor."""
        return ReactorsF._r(self)

    @r.setter
    def r(self, arg: float):
        ReactorsF._r_write(self, arg)

    @property
    def rp(self) -> float:
        """Gets/Sets the Resistance in parallel with R and X (the entire branch) for the active reactor."""
        return ReactorsF._rp(self)

    @rp.setter
    def rp(self, arg: float):
        ReactorsF._rp_write(self, arg)

    @property
    def x(self) -> float:
        """Gets/Sets the Reactance, each phase, ohms at base frequency for the active reactor."""
        return ReactorsF._x(self)

    @x.setter
    def x(self, arg: float):
        ReactorsF._x_write(self, arg)

    @property
    def name(self) -> str:
        """Gets/Sets the name of the active reactor.
        """
        return ReactorsS._name(self)

    @name.setter
    def name(self, argument: str):
        ReactorsS._name_write(self, argument)

    @property
    def l_curve(self) -> str:
        """Gets/Sets the name of XYCurve object describing per-unit variation of phase inductance, L=X/w, vs. frequency for the active reactor.
        """
        return ReactorsS._l_curve(self)

    @l_curve.setter
    def l_curve(self, arg: str):
        ReactorsS._l_curve_write(self, arg)

    @property
    def r_curve(self) -> str:
        """Gets/Sets the Name of XYCurve object, previously defined, describing per-unit variation of phase resistance, R, vs. frequency for the active reactor.
        """
        return ReactorsS._r_curve(self)

    @r_curve.setter
    def r_curve(self, arg: str):
        ReactorsS._r_curve_write(self, arg)

    @property
    def names(self) -> List[str]:
        """This parameter returns a pointer to an array of bytes containing the names of all the existing WindGens in the project. Each string is separated by a NULL (0) character."""
        return ReactorsV._names(self)

    @property
    def rmatrix(self) -> List[float]:
        """Gets/Sets the Resistance matrix, lower triangle, ohms at base frequency for the active reactor."""
        return ReactorsV._rmatrix_read(self)

    @rmatrix.setter
    def rmatrix(self, arg: List[float]):
        ReactorsV._rmatrix_write(self, arg)

    @property
    def xmatrix(self) -> List[float]:
        """Gets/Sets the Inductance matrix, lower triangle, ohms at base frequency for the active reactor."""
        return ReactorsV._xmatrix_read(self)

    @xmatrix.setter
    def xmatrix(self, arg: List[float]):
        ReactorsV._xmatrix_write(self, arg)

    @property
    def z(self) -> List[float]:
        """Alternative way of getting/setting R and X properties for the active reactor using a 2-element array of doubles."""
        return ReactorsV._z_read(self)

    @z.setter
    def z(self, arg: List[float]):
        ReactorsV._z_write(self, arg)

    @property
    def z0(self) -> List[float]:
        """Gets/Sets the Zer0-sequence impedance, ohms, as a 2-element array representing a complex number for the active reactor."""
        return ReactorsV._z0_read(self)

    @z0.setter
    def z0(self, arg: List[float]):
        ReactorsV._z0_write(self, arg)

    @property
    def z1(self) -> List[float]:
        """Gets/Sets the Positive-sequence impedance, ohms, as a 2-element array representing a complex number for the active reactor."""
        return ReactorsV._z1_read(self)

    @z1.setter
    def z1(self, arg: List[float]):
        ReactorsV._z1_write(self, arg)

    @property
    def z2(self) -> List[float]:
        """Gets/Sets the Negative-sequence impedance, ohms, as a 2-element array representing a complex number for the active reactor."""
        return ReactorsV._z2_read(self)

    @z2.setter
    def z2(self, arg: List[float]):
        ReactorsV._z2_write(self, arg)


