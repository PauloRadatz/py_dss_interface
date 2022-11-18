# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Solution.SolutionF import SolutionF
from py_dss_interface.models.Solution.SolutionI import SolutionI
from py_dss_interface.models.Solution.SolutionS import SolutionS
from py_dss_interface.models.Solution.SolutionV import SolutionV
from typing import List


class Solution(SolutionI, SolutionF, SolutionS, SolutionV):
    """
    This interface implements the Solution (ISolution) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: SolutionI, SolutionF, SolutionS, SolutionV.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def frequency(self) -> float:
        return SolutionF._frequency_read(self)

    @frequency.setter
    def frequency(self, arg: float):
        SolutionF._frequency_write(self, arg)

    @property
    def seconds(self) -> float:
        return SolutionF._seconds_read(self)

    @seconds.setter
    def seconds(self, arg: float):
        SolutionF._seconds_write(self, arg)

    @property
    def step_size(self) -> float:
        return SolutionF._step_size_read(self)

    @step_size.setter
    def step_size(self, arg: float):
        SolutionF._step_size_write(self, arg)

    @property
    def load_mult(self) -> float:
        return SolutionF._load_mult_read(self)

    @load_mult.setter
    def load_mult(self, arg: float):
        SolutionF._load_mult_write(self, arg)

    @property
    def tolerance(self) -> float:
        return SolutionF._tolerance_read(self)

    @tolerance.setter
    def tolerance(self, arg: float):
        SolutionF._tolerance_write(self, arg)

    @property
    def pct_growth(self) -> float:
        return SolutionF._pct_growth_read(self)

    @pct_growth.setter
    def pct_growth(self, arg: float):
        SolutionF._pct_growth_write(self, arg)

    @property
    def gen_kw(self) -> float:
        return SolutionF._gen_kw_read(self)

    @gen_kw.setter
    def gen_kw(self, arg: float):
        SolutionF._gen_kw_write(self, arg)

    @property
    def gen_pf(self) -> float:
        return SolutionF._gen_pf_read(self)

    @gen_pf.setter
    def gen_pf(self, arg: float):
        SolutionF._gen_pf_write(self, arg)

    @property
    def cap_kvar(self) -> float:
        return SolutionF._cap_kvar_read(self)

    @cap_kvar.setter
    def cap_kvar(self, arg: float):
        SolutionF._cap_kvar_write(self, arg)

    @property
    def gen_mult(self) -> float:
        return SolutionF._gen_mult_read(self)

    @gen_mult.setter
    def gen_mult(self, arg: float):
        SolutionF._gen_mult_write(self, arg)

    @property
    def dbl_hour(self) -> float:
        return SolutionF._dbl_hour_read(self)

    @dbl_hour.setter
    def dbl_hour(self, arg: float):
        SolutionF._dbl_hour_write(self, arg)

    @property
    def step_size_min(self) -> float:
        return SolutionF._step_size_min(self)

    @property
    def step_size_hr(self) -> float:
        return SolutionF._step_size_hr(self)

    @property
    def process_time(self) -> float:
        return SolutionF._process_time(self)

    @property
    def total_time(self) -> float:
        return SolutionF._total_time_read(self)

    @total_time.setter
    def total_time(self, arg: float):
        SolutionF._total_time_write(self, arg)

    @property
    def process_time_step(self) -> float:
        return SolutionF._process_time_step(self)

    def solve(self) -> int:
        return SolutionI._solve(self)

    @property
    def mode(self) -> int:
        return SolutionI._mode_read(self)

    @mode.setter
    def mode(self, arg: int):
        SolutionI._mode_write(self, arg)

    @property
    def hour(self) -> int:
        return SolutionI._hour_read(self)

    @hour.setter
    def hour(self, arg: int):
        SolutionI._hour_write(self, arg)

    @property
    def year(self) -> int:
        return SolutionI._year_read(self)

    @year.setter
    def year(self, arg: int):
        SolutionI._year_write(self, arg)

    @property
    def iterations(self) -> int:
        return SolutionI._iterations(self)

    @property
    def max_iterations(self) -> int:
        return SolutionI._max_iterations_read(self)

    @max_iterations.setter
    def max_iterations(self, arg: int):
        SolutionI._max_iterations_write(self, arg)

    @property
    def number(self) -> int:
        return SolutionI._number_read(self)

    @number.setter
    def number(self, arg: int):
        SolutionI._number_write(self, arg)

    @property
    def random(self) -> int:
        return SolutionI._random_read(self)

    @random.setter
    def random(self, arg: int):
        SolutionI._random_write(self, arg)

    @property
    def load_model(self) -> int:
        return SolutionI._load_model_read(self)

    @load_model.setter
    def load_model(self, arg: int):
        SolutionI._load_model_write(self, arg)

    @property
    def add_type(self) -> int:
        return SolutionI._add_type_read(self)

    @add_type.setter
    def add_type(self, arg: int):
        SolutionI._add_type_write(self, arg)

    @property
    def algorithm(self) -> int:
        return SolutionI._algorithm_read(self)

    @algorithm.setter
    def algorithm(self, arg: int):
        SolutionI._algorithm_write(self, arg)

    @property
    def control_mode(self) -> int:
        return SolutionI._control_mode_read(self)

    @control_mode.setter
    def control_mode(self, arg: int):
        SolutionI._control_mode_write(self, arg)

    @property
    def control_iterations(self) -> int:
        return SolutionI._control_iterations_read(self)

    @control_iterations.setter
    def control_iterations(self, arg: int):
        SolutionI._control_iterations_write(self, arg)

    @property
    def max_control_iterations(self) -> int:
        return SolutionI._max_control_iterations_read(self)

    @max_control_iterations.setter
    def max_control_iterations(self, arg: int):
        SolutionI._max_control_iterations_write(self, arg)

    def sample_do_control_actions(self) -> int:
        return SolutionI._sample_do_control_actions(self)

    def check_fault_status(self) -> int:
        return SolutionI._check_fault_status(self)

    def solve_direct(self) -> int:
        return SolutionI._solve_direct(self)

    def solve_power_flow(self) -> int:
        return SolutionI._solve_power_flow(self)

    def solve_no_control(self) -> int:
        return SolutionI._solve_no_control(self)

    def solve_plus_control(self) -> int:
        return SolutionI._solve_plus_control(self)

    def init_snap(self) -> int:
        return SolutionI._init_snap(self)

    def check_controls(self) -> int:
        return SolutionI._check_controls(self)

    def sample_control_devices(self) -> int:
        return SolutionI._sample_control_devices(self)

    def do_control_actions(self) -> int:
        return SolutionI._do_control_actions(self)

    def build_y_matrix(self) -> int:
        return SolutionI._build_y_matrix(self)

    @property
    def system_y_changed(self) -> int:
        return SolutionI._system_y_changed(self)

    @property
    def converged(self) -> int:
        return SolutionI._converged_read(self)

    @converged.setter
    def converged(self, arg: int):
        SolutionI._converged_write(self, arg)

    @property
    def total_iterations(self) -> int:
        return SolutionI._total_iterations(self)

    @property
    def most_iterations_done(self) -> int:
        return SolutionI._most_iterations_done(self)

    @property
    def control_actions_done(self) -> int:
        return SolutionI._control_actions_done_read(self)

    @control_actions_done.setter
    def control_actions_done(self, arg: int):
        SolutionI._control_actions_done_write(self, arg)

    def finish_time_step(self) -> int:
        return SolutionI._finish_time_step(self)

    def clean_up(self) -> int:
        return SolutionI._clean_up(self)

    def solve_all(self) -> int:
        return SolutionI._solve_all(self)

    def calc_inc_matrix(self) -> int:
        return SolutionI._calc_inc_matrix(self)

    def calc_inc_matrix_0(self) -> int:
        return SolutionI._calc_inc_matrix_0(self)

    @property
    def mode_id(self) -> str:
        return SolutionS._mode_id(self)

    @property
    def ld_curve(self) -> str:
        return SolutionS._ld_curve_read(self)

    @ld_curve.setter
    def ld_curve(self, arg: str):
        SolutionS._ld_curve_write(self, arg)

    @property
    def default_daily(self) -> str:
        return SolutionS._default_daily_read(self)

    @default_daily.setter
    def default_daily(self, arg: str):
        SolutionS._default_daily_write(self, arg)

    @property
    def default_yearly(self) -> str:
        return SolutionS._default_yearly_read(self)

    @default_yearly.setter
    def default_yearly(self, arg: str):
        SolutionS._default_yearly_write(self, arg)

    @property
    def event_log(self) -> List[str]:
        return SolutionV._event_log(self)

    @property
    def nc_matrix(self) -> List[int]:
        return SolutionV._nc_matrix(self)

    @property
    def bus_levels(self) -> List[int]:
        return SolutionV._bus_levels(self)

    @property
    def inc_matrix_rows(self) -> List[str]:
        return SolutionV._inc_matrix_rows(self)

    @property
    def inc_matrix_cols(self) -> List[str]:
        return SolutionV._inc_matrix_cols(self)

    @property
    def laplacian(self) -> List[int]:
        return SolutionV._laplacian(self)
