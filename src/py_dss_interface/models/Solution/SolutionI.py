# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class SolutionI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t SolutionI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def solution_solve(self) -> int:
        """Solution for the present solution mode. Returns 0. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def solution_read_mode(self) -> int:
        """Returns the present solution mode (See DSS help). """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def solution_write_mode(self, argument) -> int:
        """Modifies the present solution mode (See DSS help). """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(2), ctypes.c_int32(argument)))

    def solution_read_hour(self) -> int:
        """Returns the present hour (See DSS help). """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(3), ctypes.c_int32(0)))

    def solution_write_hour(self, argument) -> int:
        """Modifies the present hour (See DSS help). """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(4), ctypes.c_int32(argument)))

    def solution_read_year(self) -> int:
        """Returns the present Year (See DSS help). """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(5), ctypes.c_int32(0)))

    def solution_write_year(self, argument) -> int:
        """Modifies the present Year (See DSS help). """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(6), ctypes.c_int32(argument)))

    def solution_iterations(self) -> int:
        """Returns the number of iterations taken for the last solution. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(7), ctypes.c_int32(0)))

    def solution_read_max_iterations(self) -> int:
        """Returns the Maximum number of iterations used to solve the circuit. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(8), ctypes.c_int32(0)))

    def solution_write_max_iterations(self, argument) -> int:
        """Modifies the Maximum number of iterations used to solve the circuit. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(9), ctypes.c_int32(argument)))

    def solution_read_number(self) -> int:
        """Returns the number of solutions to perform for MonteCarlo and time series simulations. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(10), ctypes.c_int32(0)))

    def solution_write_number(self, argument) -> int:
        """Modifies the number of solutions to perform for MonteCarlo and time series simulations. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(11), ctypes.c_int32(argument)))

    def solution_read_random(self) -> int:
        """Returns the randomization mode for random variables "Gaussian" o "Uniform". """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(12), ctypes.c_int32(0)))

    def solution_write_random(self, argument) -> int:
        """Modifies the randomization mode for random variables "Gaussian" o "Uniform". """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(13), ctypes.c_int32(argument)))

    def solution_read_load_model(self) -> int:
        """Returns the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(14), ctypes.c_int32(0)))

    def solution_write_load_model(self, argument) -> int:
        """Modifies the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(15), ctypes.c_int32(argument)))

    def solution_read_add_type(self) -> int:
        """Returns the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(16), ctypes.c_int32(0)))

    def solution_write_add_type(self, argument) -> int:
        """Modifies the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(17), ctypes.c_int32(argument)))

    def solution_read_algorithm(self) -> int:
        """Returns the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(18), ctypes.c_int32(0)))

    def solution_write_algorithm(self, argument) -> int:
        """Modifies the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(19), ctypes.c_int32(argument)))

    def solution_read_control_mode(self) -> int:
        """Returns the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(20), ctypes.c_int32(0)))

    def solution_write_control_mode(self, argument) -> int:
        """Modifies the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(21), ctypes.c_int32(argument)))

    def solution_read_control_iterations(self) -> int:
        """Returns the current value of the control iteration counter. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(22), ctypes.c_int32(0)))

    def solution_write_control_iterations(self, argument) -> int:
        """Modifies the current value of the control iteration counter. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(23), ctypes.c_int32(argument)))

    def solution_read_max_control_iterations(self) -> int:
        """Returns the maximum allowable control iterations."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(24), ctypes.c_int32(0)))

    def solution_write_max_control_iterations(self, argument) -> int:
        """Modifies the maximum allowable control iterations. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(25), ctypes.c_int32(argument)))

    def solution_sample_do_control_actions(self) -> int:
        """Sample controls and then process the control queue for present control mode and dispatch control actions.
        Returns 0."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(26), ctypes.c_int32(0)))

    def solution_check_fault_status(self) -> int:
        """Executes status check on all fault objects defined in the circuit. Returns 0."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(27), ctypes.c_int32(0)))

    def solution_solve_direct(self) -> int:
        """Executes a direct solution from the system Y matrix, ignoring compensation currents of loads, generators
        (includes Yprim only)."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(28), ctypes.c_int32(0)))

    def solution_solve_power_flow(self) -> int:
        """Solves using present power flow method. Iterative solution rather than direct solution. """
        return int(self.dss_obj.SolutionI(ctypes.c_int32(29), ctypes.c_int32(0)))

    def solution_solve_no_control(self) -> int:
        """Is similar to SolveSnap except no control actions are checked or executed."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(30), ctypes.c_int32(0)))

    def solution_solve_plus_control(self) -> int:
        """Executes a power flow solution (SolveNoControl) plus executes a CheckControlActions that executes any
        pending control actions."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(31), ctypes.c_int32(0)))

    def solution_init_snap(self) -> int:
        """Initializes some variables for snap shot power flow. SolveSnap does this automatically."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(32), ctypes.c_int32(0)))

    def solution_check_controls(self) -> int:
        """Performs the normal process for sampling and executing Control Actions and Fault Status and rebuilds Y
        if necessary."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(33), ctypes.c_int32(0)))

    def solution_sample_control_devices(self) -> int:
        """Executes a sampling of all intrinsic control devices, which push control actions into the control queue."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(34), ctypes.c_int32(0)))

    def solution_do_control_actions(self) -> int:
        """Pops control actions off the control queue and dispatches to the proper control element."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(35), ctypes.c_int32(0)))

    def solution_build_y_matrix(self) -> int:
        """Forces building of the System Y matrix according to the argument:
        {1= series elements only | 2= Whole Y matrix}."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(36), ctypes.c_int32(0)))

    def solution_system_y_changed(self) -> int:
        """Indicates if elements of the System Y have been changed by recent activity. If changed returns 1;
        otherwise 0."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(37), ctypes.c_int32(0)))

    def solution_read_converged(self) -> int:
        """Indicates whether the circuit solution converged (1 converged | 0 not converged)."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(38), ctypes.c_int32(0)))

    def solution_write_converged(self, argument) -> int:
        """Modifies the converged flag (1 converged | 0 not converged). """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(39), ctypes.c_int32(argument)))

    def solution_total_iterations(self) -> int:
        """Returns the total iterations including control iterations for most recent solution."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(40), ctypes.c_int32(0)))

    def solution_most_iterations_done(self) -> int:
        """Returns the max number of iterations required to converge at any control iteration of the most recent
        solution.."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(41), ctypes.c_int32(0)))

    def solution_read_control_actions_done(self) -> int:
        """Indicates that the control actions are done: {1 done, 0 not done}."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(42), ctypes.c_int32(0)))

    def solution_write_control_actions_done(self, argument) -> int:
        """Modifies the flag to indicate that the control actions are done: {1 done, 0 not done}. """
        argument = Base.check_int_param(argument)
        return int(self.dss_obj.SolutionI(ctypes.c_int32(43), ctypes.c_int32(argument)))

    def solution_finish_time_step(self) -> int:
        """Calls cleanup, sample monitors, and increment time at end of time step."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(44), ctypes.c_int32(0)))

    def solution_clean_up(self) -> int:
        """Update storage, invcontrol, etc., at end of time step."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(45), ctypes.c_int32(0)))

    def solution_solve_all(self) -> int:
        """Starts the solution process for all the actors created in memory.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
        command."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(46), ctypes.c_int32(0)))

    def solution_calc_inc_matrix(self) -> int:
        """Starts the calculation of the incidence matrix for the active actor.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
        command."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(47), ctypes.c_int32(0)))

    # TODO include in test
    def solution_calc_inc_matrix_0(self) -> int:
        """Starts the calculation of the Branch to Node incidence matrix for the active actor.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before
        using this command. The difference between this command and the CalcIncMatrix is that the calculated matrix
        will be ordered hierarchically from the substation to the feeder end, which can be helpful for many operations.
        Additionally, the Bus Levels vector is calculated and the rows (PDElements) and columns (Buses) are permuted so
        it is easy to identify their position in the circuit."""
        return int(self.dss_obj.SolutionI(ctypes.c_int32(48), ctypes.c_int32(0)))
