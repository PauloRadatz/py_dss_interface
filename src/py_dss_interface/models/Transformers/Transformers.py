# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Transformers.TransformersF import TransformersF
from py_dss_interface.models.Transformers.TransformersI import TransformersI
from py_dss_interface.models.Transformers.TransformersS import TransformersS
from py_dss_interface.models.Transformers.TransformersV import TransformersV
from typing import List


class Transformers(TransformersV, TransformersF, TransformersI, TransformersS):
    """
    This interface implements the Transformers (ITransformer) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: TransformersV, TransformersF, TransformersI, TransformersS.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def r(self) -> float:
        """Gets the active winding resistance in %.

        Sets the active winding resistance in %."""
        return TransformersF._r_read(self)

    @r.setter
    def r(self, argument: float):
        TransformersF._r_write(self, argument)

    @property
    def tap(self) -> float:
        """Gets the active winding tap in per-unit.

        Sets the active winding tap in per-unit."""
        return TransformersF._tap_read(self)

    @tap.setter
    def tap(self, argument: float):
        TransformersF._tap_write(self, argument)

    @property
    def min_tap(self) -> float:
        """Gets the active winding minimum tap in per-unit.

        Sets the active winding minimum tap in per-unit."""
        return TransformersF._min_tap_read(self)

    @min_tap.setter
    def min_tap(self, argument: float):
        TransformersF._min_tap_write(self, argument)

    @property
    def max_tap(self) -> float:
        """Gets the active winding maximum tap in per-unit.

        Sets the active winding maximum tap in per-unit."""
        return TransformersF._max_tap_read(self)

    @max_tap.setter
    def max_tap(self, argument: float):
        TransformersF._max_tap_write(self, argument)

    @property
    def kv(self) -> float:
        """Gets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer.

        Sets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        return TransformersF._kv_read(self)

    @kv.setter
    def kv(self, argument: float):
        TransformersF._kv_write(self, argument)

    @property
    def kva(self) -> float:
        """Gets the active winding kVA rating. On winding 1, this also determines normal and emergency current
                ratings for all windings.

        Sets the active winding kVA rating. On winding 1, this also determines normal and emergency current
        ratings for all windings. """
        return TransformersF._kva_read(self)

    @kva.setter
    def kva(self, argument: float):
        TransformersF._kva_write(self, argument)

    @property
    def x_neut(self) -> float:
        """Gets the active winding neutral reactance [ohms] for wye connections.

        Sets the active winding neutral reactance [ohms] for wye connections."""
        return TransformersF._x_neut_read(self)

    @x_neut.setter
    def x_neut(self, argument: float):
        TransformersF._x_neut_write(self, argument)

    @property
    def r_neut(self) -> float:
        """Gets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye.

        Sets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        return TransformersF._r_neut_read(self)

    @r_neut.setter
    def r_neut(self, argument: float):
        TransformersF._r_neut_write(self, argument)

    @property
    def xhl(self) -> float:
        """Gets the percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2 winding or 3 winding
                transformers.

        Sets the percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2 winding or 3 winding
        transformers. """
        return TransformersF._xhl_read(self)

    @xhl.setter
    def xhl(self, argument: float):
        TransformersF._xhl_write(self, argument)

    @property
    def xht(self) -> float:
        """Gets the percent reactance between windings 1 and 3, on winding 1 kVA base. Use for 3 winding transformers
                only.

        Sets the percent reactance between windings 1 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only. """
        return TransformersF._xht_read(self)

    @xht.setter
    def xht(self, argument: float):
        TransformersF._xht_write(self, argument)

    @property
    def xlt(self) -> float:
        """Gets he percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3 winding transformers
                only.

        Sets the percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only. """
        return TransformersF._xlt_read(self)

    @xlt.setter
    def xlt(self, argument: float):
        TransformersF._xlt_write(self, argument)

    @property
    def num_windings(self) -> int:
        """Gets the number of windings on this transformer. Allocates memory; set or change this property first.

        Sets the number of windings on this transformer. Allocates memory; set or change this property first."""
        return TransformersI._num_windings_read(self)

    @num_windings.setter
    def num_windings(self, argument: int):
        TransformersI._num_windings_write(self, argument)

    @property
    def wdg(self) -> int:
        """Gets the active winding number from 1..NumWindings.
                Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.).

        Sets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        return TransformersI._wdg_read(self)

    @wdg.setter
    def wdg(self, argument: int):
        TransformersI._wdg_write(self, argument)

    @property
    def num_taps(self) -> int:
        """Gets the active winding number of tap steps between MinTap and MaxTap.

        Sets the active winding number of tap steps between MinTap and MaxTap."""
        return TransformersI._num_taps_read(self)

    @num_taps.setter
    def num_taps(self, argument: int):
        TransformersI._num_taps_write(self, argument)

    @property
    def is_delta(self) -> int:
        """Gets the information about if the active winding is delta (1) or wye (0) connection.

        Sets the information about if the active winding is delta (1) or wye (0) connection."""
        return TransformersI._is_delta_read(self)

    @is_delta.setter
    def is_delta(self, argument: int):
        TransformersI._is_delta_write(self, argument)

    def first(self) -> int:
        """Sets the first Transformer active. Return 0 if no more."""
        return TransformersI._first(self)

    def next(self) -> int:
        """Sets the next Transformer active. Return 0 if no more."""
        return TransformersI._next(self)

    @property
    def count(self) -> int:
        """Gets the number of transformers within the active circuit."""
        return TransformersI._count(self)

    @property
    def xfmr_code(self) -> str:
        """Gets the name of an XfrmCode that supplies electrical parameters for this transformer.

        Sets the name of an XfrmCode that supplies electrical parameters for this transformer."""
        return TransformersS._xfmr_code_read(self)

    @xfmr_code.setter
    def xfmr_code(self, argument: str):
        TransformersS._xfmr_code_write(self, argument)

    @property
    def name(self) -> str:
        """Gets the active transformer name.

        Sets the active transformer by name."""
        return TransformersS._name_read(self)

    @name.setter
    def name(self, argument: str):
        TransformersS._name_write(self, argument)

    @property
    def str_wdg_voltages(self) -> str:
        """Gets the voltages at the active winding of the active transformer in string format."""
        return TransformersS._str_wdg_voltages(self)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings with all Transformer names in the active circuit."""
        return TransformersV._names(self)

    @property
    def wdg_voltages(self) -> List[float]:
        """Gets a variant array of doubles containing the voltages at the active winding on the active transformer.
                These voltages come as complex pairs."""
        return TransformersV._wdg_voltages(self)

    @property
    def wdg_currents(self) -> List[float]:
        """Gets a a variant array of doubles containing the currents at the active winding on the active transformer.
                These currents come as complex pairs."""
        return TransformersV._wdg_currents(self)


