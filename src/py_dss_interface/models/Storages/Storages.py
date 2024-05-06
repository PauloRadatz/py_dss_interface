# -*- coding: utf-8 -*-
# @Time    : 5/6/2024 7:31 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : Storages.py
# @Software: PyCharm

from py_dss_interface.models.Storages.StoragesF import StoragesF
from py_dss_interface.models.Storages.StoragesS import StoragesS
from py_dss_interface.models.Storages.StoragesV import StoragesV
from py_dss_interface.models.Storages.StoragesI import StoragesI
from typing import List


class Storages(StoragesI, StoragesS, StoragesF, StoragesV):
    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def first(self) -> int:
        return StoragesI._first(self)

    def next(self) -> int:
        return StoragesI._next(self)

    @property
    def count(self) -> int:
        return StoragesI._count(self)

    @property
    def idx(self) -> int:
        return StoragesI._idx_read(self)

    @idx.setter
    def idx(self, value: int):
        StoragesI._idx_write(self, value)

    @property
    def state(self) -> int:
        return StoragesI._state_read(self)

    @state.setter
    def state(self, value: int):
        StoragesI._state_write(self, value)

    @property
    def control_mode(self) -> int:
        return StoragesI._control_mode_read(self)

    @control_mode.setter
    def control_mode(self, value: int):
        StoragesI._control_mode_write(self, value)

    @property
    def safe_mode(self) -> int:
        return StoragesI._safe_mode(self)

    @property
    def var_follow_inverter(self) -> int:
        return StoragesI._var_follow_inverter_read(self)

    @var_follow_inverter.setter
    def var_follow_inverter(self, value: int):
        StoragesI._var_follow_inverter_write(self, value)

    @property
    def names(self) -> List[str]:
        return StoragesV._names(self)

    @property
    def register_names(self) -> List[str]:
        return StoragesV._register_names(self)

    @property
    def register_values(self) -> List[float]:
        return StoragesV._register_values(self)

    @property
    def pu_soc(self) -> float:
        return StoragesF._pu_soc_read(self)

    @pu_soc.setter
    def pu_soc(self, value: float):
        StoragesF._pu_soc_write(self, value)

    @property
    def amp_limit(self) -> float:
        return StoragesF._amp_limit_read(self)

    @amp_limit.setter
    def amp_limit(self, value: float):
        StoragesF._amp_limit_write(self, value)

    @property
    def amp_limit_gain(self) -> float:
        return StoragesF._amp_limit_gain_read(self)

    @amp_limit_gain.setter
    def amp_limit_gain(self, value: float):
        StoragesF._amp_limit_gain_write(self, value)

    @property
    def charge_trigger(self) -> float:
        return StoragesF._charge_trigger_read(self)

    @charge_trigger.setter
    def charge_trigger(self, value: float):
        StoragesF._charge_trigger_write(self, value)

    @property
    def discharge_trigger(self) -> float:
        return StoragesF._discharge_trigger_read(self)

    @discharge_trigger.setter
    def discharge_trigger(self, value: float):
        StoragesF._discharge_trigger_write(self, value)

    @property
    def eff_charge(self) -> float:
        return StoragesF._eff_charge_read(self)

    @eff_charge.setter
    def eff_charge(self, value: float):
        StoragesF._eff_charge_write(self, value)

    @property
    def eff_discharge(self) -> float:
        return StoragesF._eff_discharge_read(self)

    @eff_discharge.setter
    def eff_discharge(self, value: float):
        StoragesF._eff_discharge_write(self, value)

    @property
    def kp(self) -> float:
        return StoragesF._kp_read(self)

    @kp.setter
    def kp(self, value: float):
        StoragesF._kp_write(self, value)

    @property
    def kv(self) -> float:
        return StoragesF._kv_read(self)

    @kv.setter
    def kv(self, value: float):
        StoragesF._kv_write(self, value)

    @property
    def kva(self) -> float:
        return StoragesF._kva_read(self)

    @kva.setter
    def kva(self, value: float):
        StoragesF._kva_write(self, value)

    @property
    def kvar(self) -> float:
        return StoragesF._kvar_read(self)

    @kvar.setter
    def kvar(self, value: float):
        StoragesF._kvar_write(self, value)

    @property
    def kvdc(self) -> float:
        return StoragesF._kvdc_read(self)

    @kvdc.setter
    def kvdc(self, value: float):
        StoragesF._kvdc_write(self, value)

    @property
    def kw(self) -> float:
        return StoragesF._kw_read(self)

    @kw.setter
    def kw(self, value: float):
        StoragesF._kw_write(self, value)

    @property
    def kwh_rated(self) -> float:
        return StoragesF._kwh_rated_read(self)

    @kwh_rated.setter
    def kwh_rated(self, value: float):
        StoragesF._kwh_rated_write(self, value)

    @property
    def kw_rated(self) -> float:
        return StoragesF._kw_rated_read(self)

    @kw_rated.setter
    def kw_rated(self, value: float):
        StoragesF._kw_rated_write(self, value)

    @property
    def limit_current(self) -> float:
        return StoragesF._limit_current_read(self)

    @limit_current.setter
    def limit_current(self, value: float):
        StoragesF._limit_current_write(self, value)

    @property
    def pf(self) -> float:
        return StoragesF._pf_read(self)

    @pf.setter
    def pf(self, value: float):
        StoragesF._pf_write(self, value)

    @property
    def pi_tol(self) -> float:
        return StoragesF._pi_tol_read(self)

    @pi_tol.setter
    def pi_tol(self, value: float):
        StoragesF._pi_tol_write(self, value)

    @property
    def safe_voltage(self) -> float:
        return StoragesF._safe_voltage_read(self)

    @safe_voltage.setter
    def safe_voltage(self, value: float):
        StoragesF._safe_voltage_write(self, value)

    @property
    def time_charge_trig(self) -> float:
        return StoragesF._time_charge_trig_read(self)

    @time_charge_trig.setter
    def time_charge_trig(self, value: float):
        StoragesF._time_charge_trig_write(self, value)

    @property
    def name(self) -> str:
        return StoragesS._name_read(self)

    @name.setter
    def name(self, value: str):
        StoragesS._name_write(self, value)
