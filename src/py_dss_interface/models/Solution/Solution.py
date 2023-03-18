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
        """Returns the frequency for the next solution.
        Sets the frequency for the next solution."""
        return SolutionF._frequency_read(self)

    @frequency.setter
    def frequency(self, arg: float):
        SolutionF._frequency_write(self, arg)

    @property
    def seconds(self) -> float:
        """Returns the seconds from top of the hour.
        Sets the seconds from top of the hour."""
        return SolutionF._seconds_read(self)

    @seconds.setter
    def seconds(self, arg: float):
        SolutionF._seconds_write(self, arg)

    @property
    def step_size(self) -> float:
        """Returns the step size for the next solution.
        Sets the step size for the next solution."""
        return SolutionF._step_size_read(self)

    @step_size.setter
    def step_size(self, arg: float):
        SolutionF._step_size_write(self, arg)

    @property
    def load_mult(self) -> float:
        """Returns the default load multiplier applied to all non-fixed loads.
        Sets the default load multiplier applied to all non-fixed loads."""
        return SolutionF._load_mult_read(self)

    @load_mult.setter
    def load_mult(self, arg: float):
        SolutionF._load_mult_write(self, arg)

    @property
    def tolerance(self) -> float:
        """Returns the solution convergence tolerance.
        Sets the solution convergence tolerance."""
        return SolutionF._tolerance_read(self)

    @tolerance.setter
    def tolerance(self, arg: float):
        SolutionF._tolerance_write(self, arg)

    @property
    def pct_growth(self) -> float:
        """Returns the percent default annual load growth rate.
        Sets the percent default annual load growth rate."""
        return SolutionF._pct_growth_read(self)

    @pct_growth.setter
    def pct_growth(self, arg: float):
        SolutionF._pct_growth_write(self, arg)

    @property
    def gen_kw(self) -> float:
        """Returns the generator kW for AutoAdd mode.
        Sets the generator kW for AutoAdd mode."""
        return SolutionF._gen_kw_read(self)

    @gen_kw.setter
    def gen_kw(self, arg: float):
        SolutionF._gen_kw_write(self, arg)

    @property
    def gen_pf(self) -> float:
        """Returns the pf for generators in AutoAdd mode.
        Sets the pf for generators in AutoAdd mode."""
        return SolutionF._gen_pf_read(self)

    @gen_pf.setter
    def gen_pf(self, arg: float):
        SolutionF._gen_pf_write(self, arg)

    @property
    def cap_kvar(self) -> float:
        """Returns the capacitor kvar for adding in AutoAdd mode.
        Sets the capacitor kvar for adding in AutoAdd mode."""
        return SolutionF._cap_kvar_read(self)

    @cap_kvar.setter
    def cap_kvar(self, arg: float):
        SolutionF._cap_kvar_write(self, arg)

    @property
    def gen_mult(self) -> float:
        """Returns the default multiplier applied to generators (like LoadMult).
        Sets the default multiplier applied to generators (like LoadMult)."""
        return SolutionF._gen_mult_read(self)

    @gen_mult.setter
    def gen_mult(self, arg: float):
        SolutionF._gen_mult_write(self, arg)

    @property
    def dbl_hour(self) -> float:
        """Returns the hour as a double, including fractional part.
        Sets the hour as a double, including fractional part."""
        return SolutionF._dbl_hour_read(self)

    @dbl_hour.setter
    def dbl_hour(self, arg: float):
        SolutionF._dbl_hour_write(self, arg)

    @property
    def step_size_min(self) -> float:
        """Sets the step size in minutes."""
        return SolutionF._step_size_min(self)

    @property
    def step_size_hr(self) -> float:
        """Sets the step size in Hours."""
        return SolutionF._step_size_hr(self)

    @property
    def process_time(self) -> float:
        """Retrieves the time required (microseconds) to perform the latest solution time step,
                this time does not include the time required for sampling meters/monitors."""
        return SolutionF._process_time(self)

    @property
    def total_time(self) -> float:
        """Retrieves the accumulated time required (microseconds) to perform the simulation.
        Sets the accumulated time (microseconds) register. The new value for this register must be specified in
        the argument. """
        return SolutionF._total_time_read(self)

    @total_time.setter
    def total_time(self, arg: float):
        SolutionF._total_time_write(self, arg)

    @property
    def process_time_step(self) -> float:
        """Retrieves the time required (microseconds) to perform the latest solution time step including the time
                required for sampling meters/monitors."""
        return SolutionF._process_time_step(self)

    def solve(self) -> int:
        """Solution for the present solution mode. Returns 0. """
        return SolutionI._solve(self)

    @property
    def mode(self) -> int:
        return SolutionI._mode_read(self)

    @mode.setter
    def mode(self, arg: int):
        """Returns the present solution mode (See DSS help).
        Modifies the present solution mode (See DSS help). """
        SolutionI._mode_write(self, arg)

    @property
    def hour(self) -> int:
        """Returns the present hour (See DSS help).
        Modifies the present hour (See DSS help). """
        return SolutionI._hour_read(self)

    @hour.setter
    def hour(self, arg: int):
        SolutionI._hour_write(self, arg)

    @property
    def year(self) -> int:
        """Returns the present Year (See DSS help).
        Modifies the present Year (See DSS help). """
        return SolutionI._year_read(self)

    @year.setter
    def year(self, arg: int):
        SolutionI._year_write(self, arg)

    @property
    def iterations(self) -> int:
        """Returns the number of iterations taken for the last solution. """
        return SolutionI._iterations(self)

    @property
    def max_iterations(self) -> int:
        """Returns the Maximum number of iterations used to solve the circuit.
        Modifies the Maximum number of iterations used to solve the circuit. """
        return SolutionI._max_iterations_read(self)

    @max_iterations.setter
    def max_iterations(self, arg: int):
        SolutionI._max_iterations_write(self, arg)

    @property
    def number(self) -> int:
        """Returns the number of solutions to perform for MonteCarlo and time series simulations.
        Modifies the number of solutions to perform for MonteCarlo and time series simulations. """
        return SolutionI._number_read(self)

    @number.setter
    def number(self, arg: int):
        SolutionI._number_write(self, arg)

    @property
    def random(self) -> int:
        """Returns the randomization mode for random variables "Gaussian" o "Uniform".
        Modifies the randomization mode for random variables "Gaussian" o "Uniform". """
        return SolutionI._random_read(self)

    @random.setter
    def random(self, arg: int):
        SolutionI._random_write(self, arg)

    @property
    def load_model(self) -> int:
        """Returns the Load Model: {dssPowerFlow (default)|dssAdmittance}.
        Modifies the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        return SolutionI._load_model_read(self)

    @load_model.setter
    def load_model(self, arg: int):
        SolutionI._load_model_write(self, arg)

    @property
    def add_type(self) -> int:
        """Returns the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}.
        Modifies the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        return SolutionI._add_type_read(self)

    @add_type.setter
    def add_type(self, arg: int):
        SolutionI._add_type_write(self, arg)

    @property
    def algorithm(self) -> int:
        """Returns the base solution algorithm: {dssNormalSolve | dssNewtonSolve}.
        Modifies the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        return SolutionI._algorithm_read(self)

    @algorithm.setter
    def algorithm(self, arg: int):
        SolutionI._algorithm_write(self, arg)

    @property
    def control_mode(self) -> int:
        """Returns the mode for control devices: {dssStatic (default) | dssEvent | dssTime}.
        Modifies the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        return SolutionI._control_mode_read(self)

    @control_mode.setter
    def control_mode(self, arg: int):
        SolutionI._control_mode_write(self, arg)

    @property
    def control_iterations(self) -> int:
        """Returns the current value of the control iteration counter.
        Modifies the current value of the control iteration counter. """
        return SolutionI._control_iterations_read(self)

    @control_iterations.setter
    def control_iterations(self, arg: int):
        SolutionI._control_iterations_write(self, arg)

    @property
    def max_control_iterations(self) -> int:
        """Returns the maximum allowable control iterations.
        Modifies the maximum allowable control iterations. """
        return SolutionI._max_control_iterations_read(self)

    @max_control_iterations.setter
    def max_control_iterations(self, arg: int):
        SolutionI._max_control_iterations_write(self, arg)

    def sample_do_control_actions(self) -> int:
        """Sample controls and then process the control queue for present control mode and dispatch control actions.
                Returns 0."""
        return SolutionI._sample_do_control_actions(self)

    def check_fault_status(self) -> int:
        """Executes status check on all fault objects defined in the circuit. Returns 0."""
        return SolutionI._check_fault_status(self)

    def solve_direct(self) -> int:
        """Executes a direct solution from the system Y matrix, ignoring compensation currents of loads, generators
                (includes Yprim only)."""
        return SolutionI._solve_direct(self)

    def solve_power_flow(self) -> int:
        """Solves using present power flow method. Iterative solution rather than direct solution. """
        return SolutionI._solve_power_flow(self)

    def solve_no_control(self) -> int:
        """Is similar to SolveSnap except no control actions are checked or executed."""
        return SolutionI._solve_no_control(self)

    def solve_plus_control(self) -> int:
        """Executes a power flow solution (SolveNoControl) plus executes a CheckControlActions that executes any
                pending control actions."""
        return SolutionI._solve_plus_control(self)

    def init_snap(self) -> int:
        """Initializes some variables for snap shot power flow. SolveSnap does this automatically."""
        return SolutionI._init_snap(self)

    def check_controls(self) -> int:
        """Performs the normal process for sampling and executing Control Actions and Fault Status and rebuilds Y
                if necessary."""
        return SolutionI._check_controls(self)

    def sample_control_devices(self) -> int:
        """Executes a sampling of all intrinsic control devices, which push control actions into the control queue."""
        return SolutionI._sample_control_devices(self)

    def do_control_actions(self) -> int:
        """Pops control actions off the control queue and dispatches to the proper control element."""
        return SolutionI._do_control_actions(self)

    def build_y_matrix(self) -> int:
        """Forces building of the System Y matrix according to the argument:
                {1= series elements only | 2= Whole Y matrix}."""
        return SolutionI._build_y_matrix(self)

    @property
    def system_y_changed(self) -> int:
        """Indicates if elements of the System Y have been changed by recent activity. If changed returns 1;
                otherwise 0."""
        return SolutionI._system_y_changed(self)

    @property
    def converged(self) -> int:
        """Indicates whether the circuit solution converged (1 converged | 0 not converged).
        Modifies the converged flag (1 converged | 0 not converged). """
        return SolutionI._converged_read(self)

    @converged.setter
    def converged(self, arg: int):
        SolutionI._converged_write(self, arg)

    @property
    def total_iterations(self) -> int:
        """Returns the total iterations including control iterations for most recent solution."""
        return SolutionI._total_iterations(self)

    @property
    def most_iterations_done(self) -> int:
        """Returns the max number of iterations required to converge at any control iteration of the most recent
                solution.."""
        return SolutionI._most_iterations_done(self)

    @property
    def control_actions_done(self) -> int:
        """Indicates that the control actions are done: {1 done, 0 not done}.
        Modifies the flag to indicate that the control actions are done: {1 done, 0 not done}. """
        return SolutionI._control_actions_done_read(self)

    @control_actions_done.setter
    def control_actions_done(self, arg: int):
        SolutionI._control_actions_done_write(self, arg)

    def finish_time_step(self) -> int:
        """Calls cleanup, sample monitors, and increment time at end of time step."""
        return SolutionI._finish_time_step(self)

    def clean_up(self) -> int:
        """Update storage, invcontrol, etc., at end of time step."""
        return SolutionI._clean_up(self)

    def solve_all(self) -> int:
        """Starts the solution process for all the actors created in memory.
                Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
                command."""
        return SolutionI._solve_all(self)

    def calc_inc_matrix(self) -> int:
        """Starts the calculation of the incidence matrix for the active actor.
                Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
                command."""
        return SolutionI._calc_inc_matrix(self)

    def calc_inc_matrix_0(self) -> int:
        """Starts the calculation of the Branch to Node incidence matrix for the active actor.
                Please be sure that the circuits of each actor have been compiled and ready to be solved before
                using this command. The difference between this command and the CalcIncMatrix is that the calculated matrix
                will be ordered hierarchically from the substation to the feeder end, which can be helpful for many operations.
                Additionally, the Bus Levels vector is calculated and the rows (PDElements) and columns (Buses) are permuted so
                it is easy to identify their position in the circuit."""
        return SolutionI._calc_inc_matrix_0(self)

    @property
    def mode_id(self) -> str:
        """Returns the ID (text) of the present solution mode."""
        return SolutionS._mode_id(self)

    @property
    def ld_curve(self) -> str:
        """Returns the Load-Duration Curve name for LD modes.
        Sets the Load-Duration Curve name for LD modes."""
        return SolutionS._ld_curve_read(self)

    @ld_curve.setter
    def ld_curve(self, arg: str):
        SolutionS._ld_curve_write(self, arg)

    @property
    def default_daily(self) -> str:
        """Returns the default daily load shape (defaults to "Default").
        Sets the default daily load shape (defaults to "Default")."""
        return SolutionS._default_daily_read(self)

    @default_daily.setter
    def default_daily(self, arg: str):
        SolutionS._default_daily_write(self, arg)

    @property
    def default_yearly(self) -> str:
        """Returns the default yearly load shape (defaults to "Default").
        Sets the default yearly load shape (defaults to "Default")."""
        return SolutionS._default_yearly_read(self)

    @default_yearly.setter
    def default_yearly(self, arg: str):
        SolutionS._default_yearly_write(self, arg)

    @property
    def event_log(self) -> List[str]:
        """Returns an array of strings containing the Event Log."""
        return SolutionV._event_log(self)

    @property
    def nc_matrix(self) -> List[int]:
        """Returns an array of integers containing the incidence matrix (1-D). Each cell of the incidence matrix is
                delivered using 3 elements of the array delivered, the first is the row, the second is the column and the
                third is the value (1/-1). This procedure will only deliver the non-zero elements.. """
        return SolutionV._nc_matrix(self)

    @property
    def bus_levels(self) -> List[int]:
        """Returns an array of integers containing BusLevels array. This array gives a numeric value to each bus to
                specify how far it is from the circuit?s backbone (a continuous path from the feeder head to the feeder end).
                It is very handy to understand the circuit?s topology. """
        return SolutionV._bus_levels(self)

    @property
    def inc_matrix_rows(self) -> List[str]:
        """Returns an array of strings specifying the way the rows of the incidence matrix (PDElements) are organized,
                 depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
                 the result could be very different.."""
        return SolutionV._inc_matrix_rows(self)

    @property
    def inc_matrix_cols(self) -> List[str]:
        """Returns an array of strings specifying the way the cols of the incidence matrix (buses) are organized,
                depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
                the result could be very different."""
        return SolutionV._inc_matrix_cols(self)

    @property
    def laplacian(self) -> List[int]:
        """Returns an array of integers containing the Laplacian matrix using the incidence matrix previously calculated
                , this means that before calling this command the incidence matrix needs to be calculated using
                calcincmatrix/calcincmatrix_o. This command will return only the non-zero values in compressed coordinate
                format (row, col, value).."""
        return SolutionV._laplacian(self)
