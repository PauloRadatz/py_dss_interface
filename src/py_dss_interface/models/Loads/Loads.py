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
        """Allows to read the AllocationFactor property of the active load. The parameter argument can be filled with
                a 0.

        Allows to write the AllocationFactor property of the active load. The parameter argument must contain the
                new value in AllocationFactor for the desired active load. The return value will be equal to 0. """

        return LoadsF._allocation_factor(self)

    @allocation_factor.setter
    def allocation_factor(self, value: float):
        LoadsF._allocation_factor_write(self, value)

    @property
    def c_factor(self) -> float:
        """Allows to read the CFactor property of the active load. The parameter argument can be filled with a 0.

        Allows to write the CFactor property of the active load. The parameter argument must contain the new value
                in CFactor for the desired active load. The return value will be equal to 0. """
        return LoadsF._c_factor(self)

    @c_factor.setter
    def c_factor(self, value: float):
        LoadsF._c_factor_write(self, value)

    @property
    def class_number(self) -> int:
        """Allows to read the code number used to separate loads by class or group. The parameter argument can be
                filled with a 0.

        Allows to read the code number used to separate loads by class or group. The parameter argument can be
        filled with a 0. """
        return LoadsI._class(self)

    @class_number.setter
    def class_number(self, value: int):
        LoadsI._class_write(self, value)

    @property
    def count(self) -> int:
        """Returns the number of load elements within the active circuit. The parameter argument can be filled with a
                0. """
        return LoadsI._count(self)

    @property
    def cvr_curve(self) -> str:
        """Allows to read the CVRCurve property of the active load. The parameter argument can be filled with an
                empty string.

        Allows to set the CVRCurve property for the active load. The parameter argument must contain the Name of
                the new CVRCurve to be linked to the active load. The return value will be equal to empty. """
        return LoadsS._cvr_curve(self)

    @cvr_curve.setter
    def cvr_curve(self, value: str):
        LoadsS._cvr_curve_write(self, value)

    @property
    def cvr_vars(self) -> float:
        """Allows to read the CVRvars property of the active load. The parameter argument can be filled with a 0.

        Allows to write the CVRvars property of the active load. The parameter argument must contain the new value
                in CVRWatts for the desired active load. The return value will be equal to 0. """
        return LoadsF._cvr_vars(self)

    @cvr_vars.setter
    def cvr_vars(self, value: float):
        LoadsF._cvr_vars_write(self, value)

    @property
    def cvr_watts(self) -> float:
        """Allows to read the CVRWatts property of the active load. The parameter argument can be filled with a 0.

        Allows to write the CVRWatts property of the active load. The parameter argument must contain the new
                value in CVRWatts for the desired active load. The return value will be equal to 0. """
        return LoadsF._cvr_watts(self)

    @cvr_watts.setter
    def cvr_watts(self, value: float):
        LoadsF._cvr_watts_write(self, value)

    @property
    def daily(self) -> str:
        """Allows to read the daily property of the active load. The parameter argument can be filled with an empty
                string.

        Allows to set the daily property for the active load. The parameter argument must contain the Name of the
        new daily to be linked to the active load. The return value will be equal to empty. """
        return LoadsS._daily(self)

    @daily.setter
    def daily(self, value: str):
        LoadsS._daily_write(self, value)

    @property
    def duty(self) -> str:
        """Allows to read the duty property of the active load. The parameter argument can be filled with an empty
                string.

        Allows to set the dduty property for the active load. The parameter argument must contain the Name of the
        new duty to be linked to the active load. The return value will be equal to empty. """
        return LoadsS._duty(self)

    @duty.setter
    def duty(self, value: str):
        LoadsS._duty_write(self, value)

    def first(self) -> int:
        """Allows to set the active load into the first load registered in the active circuit. As a result,
                this property will return the number 1. The parameter argument can be filled with a 0. """
        return LoadsI._first(self)

    @property
    def growth(self) -> str:
        """Allows to read the Growth property of the active load. The parameter argument can be filled with an empty
                string.

        Allows to set the Growth property for the active load. The parameter argument must contain the Name of the
        new Growth to be linked to the active load. The return value will be equal to empty. """
        return LoadsS._growth(self)

    @growth.setter
    def growth(self, value: str):
        LoadsS._growth_write(self, value)

    @property
    def idx(self) -> int:
        """Allows to read the index of the active load. The parameter argument can be filled with a 0.

        Allows to write the index of the active load. The parameter argument must contain the index of the desired
        active load. The return value will be equal to 0."""
        return LoadsI._idx(self)

    @idx.setter
    def idx(self, value: int):
        LoadsI._idx_write(self, value)

    @property
    def is_delta(self) -> int:
        """Allows to read if the active load is connected in delta, if the answer is positive, this function will
                deliver a 1; otherwise, the answer will be 0. The parameter argument can be filled with a 0.

        Allows to read if the active load is connected in delta, if the answer is positive, this function will
        deliver a 1; otherwise, the answer will be 0. This parameter will return a 0. """
        return LoadsI._is_delta(self)

    @is_delta.setter
    def is_delta(self, value: int):
        LoadsI._is_delta_write(self, value)

    @property
    def kv(self) -> float:
        """Allows to read the kV property of the active load. The parameter argument can be filled with a 0.

        Allows to write the kV property of the active load. The parameter argument must contain the new value in
                kV for the desired active load. The return value will be equal to 0. """
        return LoadsF._kv(self)

    @kv.setter
    def kv(self, value: float):
        LoadsF._kv_write(self, value)

    @property
    def kva(self) -> float:
        """Allows to read the kva property of the active load. The parameter argument can be filled with a 0.

        Allows to write the kva property of the active load. The parameter argument must contain the new value in
                kva for the desired active load. The return value will be equal to 0. """
        return LoadsF._kva(self)

    @kva.setter
    def kva(self, value: float):
        LoadsF._kva_write(self, value)

    @property
    def kvar(self) -> float:
        """Allows to read the kvar property of the active load. The parameter argument can be filled with a 0.

        Allows to write the kvar property of the active load. The parameter argument must contain the new value in
                kvar for the desired active load. The return value will be equal to 0. """
        return LoadsF._kvar(self)

    @kvar.setter
    def kvar(self, value: float):
        LoadsF._kvar_write(self, value)

    @property
    def kw(self) -> float:
        """Allows to read the kW property of the active load. The parameter argument can be filled with a 0.

        Allows to write the kW property of the active load. The parameter argument must contain the new value in
                kW for the desired active load. The return value will be equal to 0. """
        return LoadsF._kw(self)

    @kw.setter
    def kw(self, value: float):
        LoadsF._kw_write(self, value)

    @property
    def kwh(self) -> float:
        """Allows to read the kWh property of the active load. The parameter argument can be filled with a 0.

        Allows to write the kWh property of the active load. The parameter argument must contain the new value in
        kWh for the desired active load. The return value will be equal to 0. """
        return LoadsF._kwh(self)

    @kwh.setter
    def kwh(self, value: float):
        LoadsF._kwh_write(self, value)

    @property
    def kwh_days(self) -> float:
        """Allows to read the kWhdays property of the active load. The parameter argument can be filled with a 0.

        Allows to write the kWhdays property of the active load. The parameter argument must contain the new value
        in kWhdays for the desired active load. The return value will be equal to 0. """
        return LoadsF._kwh_days(self)

    @kwh_days.setter
    def kwh_days(self, value: float):
        LoadsF._kwh_days_write(self, value)

    @property
    def model(self) -> int:
        """Allows to read the model of the active load. The parameter argument can be filled with a 0.

        Allows to write the model of the active load using the parameter argument. This parameter will return a 0."""
        return LoadsI._model(self)

    @model.setter
    def model(self, value: int):
        LoadsI._model_write(self, value)

    @property
    def name(self) -> str:
        """Allows to read the Name property of the active load. The parameter argument can be filled with an empty
                string.

        Allows to set the active load by specifying the Name load. The parameter argument must contain the Name of
        the load to activate. The return value will be equal to empty. """
        return LoadsS._name(self)

    @name.setter
    def name(self, value: str):
        LoadsS._name_write(self, value)

    @property
    def names(self):
        """Allows to read the names of all the loads present in the active circuit. The result is delivered as
                variant, however, the content of this variant is an array of strings. """
        return LoadsV._names(self)

    def next(self) -> int:
        """Sets the active load into the next load registered in the active circuit. As a result, this property will
                deliver the index of the active load. The parameter argument can be filled with a 0. """
        return LoadsI._next(self)

    @property
    def num_cust(self) -> int:
        """Allows to read the number of customer of the active load. The parameter argument can be filled with a 0.

        Allows to write the number of customers of the active load using the parameter argument. This parameter
        will return a 0. """
        return LoadsI._num_cust(self)

    @num_cust.setter
    def num_cust(self, value: int):
        LoadsI._num_cust_write(self, value)

    @property
    def pct_mean(self) -> float:
        """Allows to read the PctMean property of the active load. The parameter argument can be filled with a 0.

        Allows to write the PctMean property of the active load. The parameter argument must contain the new value
                in PctMean for the desired active load. The return value will be equal to 0. """
        return LoadsF._pct_mean(self)

    @pct_mean.setter
    def pct_mean(self, value: float):
        LoadsF._pct_mean_write(self, value)

    @property
    def pct_std_dev(self) -> float:
        """Allows to read the PctStdDev property of the active load. The parameter argument can be filled with a 0.

        Allows to write the PctStdDev property of the active load. The parameter argument must contain the new
                value in PctStdDev for the desired active load. The return value will be equal to 0. """
        return LoadsF._pct_std_dev(self)

    @pct_std_dev.setter
    def pct_std_dev(self, value: float):
        LoadsF._pct_std_dev_write(self, value)

    @property
    def pf(self) -> float:
        """Allows to read the pf property of the active load. The parameter argument can be filled with a 0.

        Allows to write the pf property of the active load. The parameter argument must contain the new value in
        pf for the desired active load. The return value will be equal to 0. """
        return LoadsF._pf(self)

    @pf.setter
    def pf(self, value: float):
        LoadsF._pf_write(self, value)

    @property
    def r_neut(self) -> float:
        """Allows to read the RNeut (neutral resistance for wye connected loads) property of the active load. The
                parameter argument can be filled with a 0.

        Allows to write the RNeut (neutral resistance for wye connected loads) property of the active load. The
        parameter argument must contain the new value in RNeut for the desired active load. The return value will be
        equal to 0. """
        return LoadsF._r_neut(self)

    @r_neut.setter
    def r_neut(self, value: float):
        LoadsF._r_neut_write(self, value)

    @property
    def rel_weight(self) -> float:
        """Allows to read the RelWeight (relative weighting factor) property of the active load. The parameter
                argument can be filled with a 0.

        Allows to write the RelWeight (relative weighting factor) property of the active load. The parameter
        argument must contain the new value in RelWeight for the desired active load. The return value will be equal
        to 0. """
        return LoadsF._rel_weight(self)

    @rel_weight.setter
    def rel_weight(self, value: float):
        LoadsF._rel_weight_write(self, value)

    @property
    def rl(self) -> float:
        """Allows to read the PctSeriesRL (Percent of Load that is modeled as series R-L for harmonic studies)
                property of the active load. The parameter argument can be filled with a 0.

        Allows to write the PctSeriesRL (Percent of Load that is modeled as series R-L for harmonic studies)
        property of the active load. The parameter argument must contain the new value in PctSeriesRL for the desired
        active load. The return value will be equal to 0. """
        return LoadsF._rl(self)

    @rl.setter
    def rl(self, value: float):
        LoadsF._rl_write(self, value)

    @property
    def spectrum(self) -> str:
        """Allows to read the Spectrum property of the active load. The parameter argument can be filled with an
                empty string.

        Allows to set the Spectrum property for the active load. The parameter argument must contain the Name of
        the new Spectrum to be linked to the active load. The return value will be equal to empty. """
        return LoadsS._spectrum(self)

    @spectrum.setter
    def spectrum(self, value: str):
        LoadsS._spectrum_write(self, value)

    @property
    def status(self) -> int:
        """Allows to read Response to load multipliers: Fixed (growth only - 1), Exempt (no LD curve - 2), Variable (
                all - 0), of the active load. The parameter argument can be filled with a 0.

        Allows to read Response to load multipliers: Fixed (growth only - 1), Exempt (no LD curve - 2), Variable (
        all - 0), of the active load. This parameter will return a 0. """
        return LoadsI._status(self)

    @status.setter
    def status(self, value: int):
        LoadsI._status_write(self, value)

    @property
    def vmax_pu(self) -> float:
        """Allows to read the VMaxpu property of the active load. The parameter argument can be filled with a 0.

        Allows to write the VMaxpu property of the active load. The parameter argument must contain the new value
                in VMaxpu for the desired active load. The return value will be equal to 0. """
        return LoadsF._vmax_pu(self)

    @vmax_pu.setter
    def vmax_pu(self, value: float):
        LoadsF._vmax_pu_write(self, value)

    @property
    def vmin_emerg(self) -> float:
        """Allows to read the VMinemerg property of the active load. The parameter argument can be filled with a 0.

        Allows to write the VMinemerg property of the active load. The parameter argument must contain the new
                value in VMinemerg for the desired active load. The return value will be equal to 0. """
        return LoadsF._vmin_emerg(self)

    @vmin_emerg.setter
    def vmin_emerg(self, value: float):
        LoadsF._vmin_emerg_write(self, value)

    @property
    def vmin_norm(self) -> float:
        """Allows to read the VMinnorm property of the active load. The parameter argument can be filled with a 0.

        Allows to write the VMinnorm property of the active load. The parameter argument must contain the new
                value in VMinnorm for the desired active load. The return value will be equal to 0. """
        return LoadsF._vmin_norm(self)

    @vmin_norm.setter
    def vmin_norm(self, value: float):
        """Allows to read the VMinpu property of the active load. The parameter argument can be filled with a 0.

        Allows to write the VMinpu property of the active load. The parameter argument must contain the new value
                in VMinpu for the desired active load. The return value will be equal to 0. """
        LoadsF._vmin_norm_write(self, value)

    @property
    def vmin_pu(self) -> float:
        return LoadsF._vmin_pu(self)

    @vmin_pu.setter
    def vmin_pu(self, value: float):
        LoadsF._vmin_pu_write(self, value)

    @property
    def x_neut(self) -> float:
        """Allows to read the Xneut property of the active load. The parameter argument can be filled with a 0.

        Allows to write the Xneut property of the active load. The parameter argument must contain the new value
                in Xneut for the desired active load. The return value will be equal to 0. """
        return LoadsF._x_neut(self)

    @x_neut.setter
    def x_neut(self, value: float):
        LoadsF._x_neut_write(self, value)

    @property
    def xfkva(self) -> float:
        """Allows to read the xfKVA (Rated service transformer KVA for load allocation, using Allocationfactor.
                Affects kW, kvar and pf.) property of the active load. The parameter argument can be filled with a 0.

        Allows to write the xfKVA (Rated service transformer KVA for load allocation, using Allocationfactor.
        Affects kW, kvar and pf.) property of the active load. The parameter argument must contain the new value in
        xfKVA for the desired active load. The return value will be equal to 0. """
        return LoadsF._xfkva(self)

    @xfkva.setter
    def xfkva(self, value: float):
        LoadsF._xfkva_write(self, value)

    @property
    def yearly(self) -> str:
        """Allows to read the Yearly property of the active load. The parameter argument can be filled with an empty
                string.

        Allows to set the Yearly property for the active load. The parameter argument must contain the Name of the
        new Yearly to be linked to the active load. The return value will be equal to empty. """
        return LoadsS._yearly(self)

    @yearly.setter
    def yearly(self, value: str):
        LoadsS._yearly_write(self, value)

    @property
    def zipv(self) -> List[float]:
        """Allows to read the array of 7 elements (doubles) for ZIP property of the active Load object.

        Allows to write the array of 7 elements (doubles) for ZIP property of the active Load object."""
        return LoadsV._zipv_read(self)

    @zipv.setter
    def zipv(self, value: List[float]):
        LoadsV._zipv_write(self, value)
