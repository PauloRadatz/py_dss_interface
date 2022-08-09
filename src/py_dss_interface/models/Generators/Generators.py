# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Generators.GeneratorsF import GeneratorsF
from py_dss_interface.models.Generators.GeneratorsI import GeneratorsI
from py_dss_interface.models.Generators.GeneratorsS import GeneratorsS
from py_dss_interface.models.Generators.GeneratorsV import GeneratorsV


class Generators(GeneratorsI, GeneratorsF, GeneratorsS, GeneratorsV):
    """
    This interface implements the Generators (IGenerators) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: GeneratorsI, GeneratorsF, GeneratorsS, GeneratorsV.
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def count(self) -> int:
        return GeneratorsI._count(self)

    @property
    def first(self) -> int:
        return GeneratorsI._first(self)

    @property
    def forced_on(self) -> int:
        return GeneratorsI._forced_on(self)

    @forced_on.setter
    def forced_on(self, value: int):
        GeneratorsI._forced_on_write(self, value)

    @property
    def idx(self) -> int:
        return GeneratorsI._idx(self)

    @idx.setter
    def idx(self, value: int):
        GeneratorsI._idx_write(self, value)

    @property
    def kv(self) -> float:
        return GeneratorsF._kv(self)

    @kv.setter
    def kv(self, value: float):
        GeneratorsF._kv_write(self, value)

    @property
    def kva_rated(self) -> float:
        return GeneratorsF._kva_rated(self)

    @kva_rated.setter
    def kva_rated(self, value: float):
        GeneratorsF._kva_rated_write(self, value)

    @property
    def kvar(self) -> float:
        return GeneratorsF._kvar(self)

    @kvar.setter
    def kvar(self, value: float):
        GeneratorsF._kvar_write(self, value)

    @property
    def kw(self) -> float:
        return GeneratorsF._kw(self)

    @kw.setter
    def kw(self, value: float):
        GeneratorsF._kw_write(self, value)

    @property
    def model(self) -> int:
        return GeneratorsI._model(self)

    @model.setter
    def model(self, value: int):
        GeneratorsI._model_write(self, value)

    @property
    def name(self) -> str:
        return GeneratorsS._name(self)

    @name.setter
    def name(self, value: str):
        GeneratorsS._name_write(self, value)

    @property
    def names(self) -> List[str]:
        return GeneratorsV._names(self)

    @property
    def next(self) -> int:
        return GeneratorsI._next(self)

    @property
    def pf(self) -> float:
        return GeneratorsF._pf(self)

    @pf.setter
    def pf(self, value: float):
        GeneratorsF._pf_write(self, value)

    @property
    def phases(self) -> int:
        return GeneratorsI._phases(self)

    @phases.setter
    def phases(self, value: int):
        GeneratorsI._phases_write(self, value)

    @property
    def register_names(self) -> List[str]:
        return GeneratorsV._register_names(self)

    @property
    def register_values(self) -> List[float]:
        return GeneratorsV._register_values(self)

    @property
    def vmax_pu(self) -> float:
        return GeneratorsF._vmax_pu(self)

    @vmax_pu.setter
    def vmax_pu(self, value: float):
        GeneratorsF._vmax_pu_write(self, value)

    @property
    def vmin_pu(self) -> float:
        return GeneratorsF._vmin_pu(self)

    @vmin_pu.setter
    def vmin_pu(self, value: float):
        GeneratorsF._vmin_pu_write(self, value)
