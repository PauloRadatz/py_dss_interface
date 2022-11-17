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
        return TransformersF._r_read(self)

    @r.setter
    def r(self, argument: float):
        TransformersF._r_write(self, argument)

    @property
    def tap(self) -> float:
        return TransformersF._tap_read(self)

    @tap.setter
    def tap(self, argument: float):
        TransformersF._tap_write(self, argument)

    @property
    def min_tap(self) -> float:
        return TransformersF._min_tap_read(self)

    @min_tap.setter
    def min_tap(self, argument: float):
        TransformersF._min_tap_write(self, argument)

    @property
    def max_tap(self) -> float:
        return TransformersF._max_tap_read(self)

    @max_tap.setter
    def max_tap(self, argument: float):
        TransformersF._max_tap_write(self, argument)

    @property
    def kv(self) -> float:
        return TransformersF._kv_read(self)

    @kv.setter
    def kv(self, argument: float):
        TransformersF._kv_write(self, argument)

    @property
    def kva(self) -> float:
        return TransformersF._kva_read(self)

    @kva.setter
    def kva(self, argument: float):
        TransformersF._kva_write(self, argument)

    @property
    def x_neut(self) -> float:
        return TransformersF._x_neut_read(self)

    @x_neut.setter
    def x_neut(self, argument: float):
        TransformersF._x_neut_write(self, argument)

    @property
    def r_neut(self) -> float:
        return TransformersF._r_neut_read(self)

    @r_neut.setter
    def r_neut(self, argument: float):
        TransformersF._r_neut_write(self, argument)

    @property
    def xhl(self) -> float:
        return TransformersF._xhl_read(self)

    @xhl.setter
    def xhl(self, argument: float):
        TransformersF._xhl_write(self, argument)

    @property
    def xht(self) -> float:
        return TransformersF._xht_read(self)

    @xht.setter
    def xht(self, argument: float):
        TransformersF._xht_write(self, argument)

    @property
    def xlt(self) -> float:
        return TransformersF._xlt_read(self)

    @xlt.setter
    def xlt(self, argument: float):
        TransformersF._xlt_write(self, argument)

    @property
    def num_windings(self) -> int:
        return TransformersI._num_windings_read(self)

    @num_windings.setter
    def num_windings(self, argument: int):
        TransformersI._num_windings_write(self, argument)

    @property
    def wdg(self) -> int:
        return TransformersI._wdg_read(self)

    @wdg.setter
    def wdg(self, argument: int):
        TransformersI._wdg_write(self, argument)

    @property
    def num_taps(self) -> int:
        return TransformersI._num_taps_read(self)

    @num_taps.setter
    def num_taps(self, argument: int):
        TransformersI._num_taps_write(self, argument)

    @property
    def is_delta(self) -> int:
        return TransformersI._is_delta_read(self)

    @is_delta.setter
    def is_delta(self, argument: int):
        TransformersI._is_delta_write(self, argument)

    def first(self) -> int:
        return TransformersI._first(self)

    def next(self) -> int:
        return TransformersI._next(self)

    @property
    def count(self) -> int:
        return TransformersI._count(self)

    @property
    def xfmr_code(self) -> str:
        return TransformersS._xfmr_code_read(self)

    @xfmr_code.setter
    def xfmr_code(self, argument: str):
        TransformersS._xfmr_code_write(self, argument)

    @property
    def name(self) -> str:
        return TransformersS._name_read(self)

    @name.setter
    def name(self, argument: str):
        TransformersS._name_write(self, argument)

    @property
    def str_wdg_voltages(self) -> str:
        return TransformersS._str_wdg_voltages(self)

    @property
    def names(self) -> List[str]:
        return TransformersV._names(self)

    @property
    def wdg_voltages(self) -> List[float]:
        return TransformersV._wdg_voltages(self)

    @property
    def wdg_currents(self) -> List[float]:
        return TransformersV._wdg_currents(self)


