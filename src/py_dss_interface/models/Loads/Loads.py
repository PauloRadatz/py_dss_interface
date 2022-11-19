# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Loads.LoadsF import LoadsF
from py_dss_interface.models.Loads.LoadsI import LoadsI
from py_dss_interface.models.Loads.LoadsS import LoadsS
from py_dss_interface.models.Loads.LoadsV import LoadsV
from typing import List


class Loads(LoadsF, LoadsI, LoadsS, LoadsV):
    """
    This interface implements the Loads (ILoads) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: LoadsF, LoadsI, LoadsS, LoadsV.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def allocation_factor(self) -> float:
        return LoadsF._allocation_factor(self)

    @allocation_factor.setter
    def allocation_factor(self, value: float):
        LoadsF._allocation_factor_write(self, value)

    @property
    def c_factor(self) -> float:
        return LoadsF._c_factor(self)

    @c_factor.setter
    def c_factor(self, value: float):
        LoadsF._c_factor_write(self, value)

    @property
    def class_number(self) -> int:
        return LoadsI._class(self)

    @class_number.setter
    def class_number(self, value: int):
        LoadsI._class_write(self, value)

    @property
    def count(self) -> int:
        return LoadsI._count(self)

    @property
    def cvr_curve(self) -> str:
        return LoadsS._cvr_curve(self)

    @cvr_curve.setter
    def cvr_curve(self, value: str):
        LoadsS._cvr_curve_write(self, value)

    @property
    def cvr_vars(self) -> float:
        return LoadsF._cvr_vars(self)

    @cvr_vars.setter
    def cvr_vars(self, value: float):
        LoadsF._cvr_vars_write(self, value)

    @property
    def cvr_watts(self) -> float:
        return LoadsF._cvr_watts(self)

    @cvr_watts.setter
    def cvr_watts(self, value: float):
        LoadsF._cvr_watts_write(self, value)

    @property
    def daily(self) -> str:
        return LoadsS._daily(self)

    @daily.setter
    def daily(self, value: str):
        LoadsS._daily_write(self, value)

    @property
    def duty(self) -> str:
        return LoadsS._duty(self)

    @duty.setter
    def duty(self, value: str):
        LoadsS._duty_write(self, value)

    def first(self) -> int:
        return LoadsI._first(self)

    @property
    def growth(self) -> str:
        return LoadsS._growth(self)

    @growth.setter
    def growth(self, value: str):
        LoadsS._growth_write(self, value)

    @property
    def idx(self) -> int:
        return LoadsI._idx(self)

    @idx.setter
    def idx(self, value: int):
        LoadsI._idx_write(self, value)

    @property
    def is_delta(self) -> int:
        return LoadsI._is_delta(self)

    @is_delta.setter
    def is_delta(self, value: int):
        LoadsI._is_delta_write(self, value)

    @property
    def kv(self) -> float:
        return LoadsF._kv(self)

    @kv.setter
    def kv(self, value: float):
        LoadsF._kv_write(self, value)

    @property
    def kva(self) -> float:
        return LoadsF._kva(self)

    @kva.setter
    def kva(self, value: float):
        LoadsF._kva_write(self, value)

    @property
    def kvar(self) -> float:
        return LoadsF._kvar(self)

    @kvar.setter
    def kvar(self, value: float):
        LoadsF._kvar_write(self, value)

    @property
    def kw(self) -> float:
        return LoadsF._kw(self)

    @kw.setter
    def kw(self, value: float):
        LoadsF._kw_write(self, value)

    @property
    def kwh(self) -> float:
        return LoadsF._kwh(self)

    @kwh.setter
    def kwh(self, value: float):
        LoadsF._kwh_write(self, value)

    @property
    def kwh_days(self) -> float:
        return LoadsF._kwh_days(self)

    @kwh_days.setter
    def kwh_days(self, value: float):
        LoadsF._kwh_days_write(self, value)

    @property
    def model(self) -> int:
        return LoadsI._model(self)

    @model.setter
    def model(self, value: int):
        LoadsI._model_write(self, value)

    @property
    def name(self) -> str:
        return LoadsS._name(self)

    @name.setter
    def name(self, value: str):
        LoadsS._name_write(self, value)

    @property
    def names(self):
        return LoadsV._names(self)

    def next(self) -> int:
        return LoadsI._next(self)

    @property
    def num_cust(self) -> int:
        return LoadsI._num_cust(self)

    @num_cust.setter
    def num_cust(self, value: int):
        LoadsI._num_cust_write(self, value)

    @property
    def pct_mean(self) -> float:
        return LoadsF._pct_mean(self)

    @pct_mean.setter
    def pct_mean(self, value: float):
        LoadsF._pct_mean_write(self, value)

    @property
    def pct_std_dev(self) -> float:
        return LoadsF._pct_std_dev(self)

    @pct_std_dev.setter
    def pct_std_dev(self, value: float):
        LoadsF._pct_std_dev_write(self, value)

    @property
    def pf(self) -> float:
        return LoadsF._pf(self)

    @pf.setter
    def pf(self, value: float):
        LoadsF._pf_write(self, value)

    @property
    def r_neut(self) -> float:
        return LoadsF._r_neut(self)

    @r_neut.setter
    def r_neut(self, value: float):
        LoadsF._r_neut_write(self, value)

    @property
    def rel_weight(self) -> float:
        return LoadsF._rel_weight(self)

    @rel_weight.setter
    def rel_weight(self, value: float):
        LoadsF._rel_weight_write(self, value)

    @property
    def rl(self) -> float:
        return LoadsF._rl(self)

    @rl.setter
    def rl(self, value: float):
        LoadsF._rl_write(self, value)

    @property
    def spectrum(self) -> str:
        return LoadsS._spectrum(self)

    @spectrum.setter
    def spectrum(self, value: str):
        LoadsS._spectrum_write(self, value)

    @property
    def status(self) -> int:
        return LoadsI._status(self)

    @status.setter
    def status(self, value: int):
        LoadsI._status_write(self, value)

    @property
    def vmax_pu(self) -> float:
        return LoadsF._vmax_pu(self)

    @vmax_pu.setter
    def vmax_pu(self, value: float):
        LoadsF._vmax_pu_write(self, value)

    @property
    def vmin_emerg(self) -> float:
        """The vmin_emerg property."""
        return LoadsF._vmin_emerg(self)

    @vmin_emerg.setter
    def vmin_emerg(self, value: float):
        LoadsF._vmin_emerg_write(self, value)

    @property
    def vmin_norm(self) -> float:
        return LoadsF._vmin_norm(self)

    @vmin_norm.setter
    def vmin_norm(self, value: float):
        LoadsF._vmin_norm_write(self, value)

    @property
    def vmin_pu(self) -> float:
        return LoadsF._vmin_pu(self)

    @vmin_pu.setter
    def vmin_pu(self, value: float):
        LoadsF._vmin_pu_write(self, value)

    @property
    def x_neut(self) -> float:
        return LoadsF._x_neut(self)

    @x_neut.setter
    def x_neut(self, value: float):
        LoadsF._x_neut_write(self, value)

    @property
    def xfkva(self) -> float:
        return LoadsF._xfkva(self)

    @xfkva.setter
    def xfkva(self, value: float):
        LoadsF._xfkva_write(self, value)

    @property
    def yearly(self) -> str:
        return LoadsS._yearly(self)

    @yearly.setter
    def yearly(self, value: str):
        LoadsS._yearly_write(self, value)

    @property
    def zipv(self) -> List[float]:
        return LoadsV._zipv_read(self)

    @zipv.setter
    def zipv(self, value: List[float]):
        LoadsV._zipv_write(self, value)
