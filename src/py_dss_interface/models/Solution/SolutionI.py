# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Solution(Base):
    """
    This interface implements the Solution (ISolution) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """
    # SolutionI (int)
    def solution_solve(self):
        """Solution for the present solution mode. Returns 0. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def solution_read_mode(self):
        """Returns the present solution mode (See DSS help). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def solution_write_mode(self, argument):
        """Modifies the present solution mode (See DSS help). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(2), ctypes.c_int32(argument)))
        return result

    def solution_read_hour(self):
        """Returns the present hour (See DSS help). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def solution_write_hour(self, argument):
        """Modifies the present hour (See DSS help). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(4), ctypes.c_int32(argument)))
        return result

    def solution_read_year(self):
        """Returns the present Year (See DSS help). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result

    def solution_write_year(self, argument):
        """Modifies the present Year (See DSS help). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(6), ctypes.c_int32(argument)))
        return result

    def solution_iterations(self):
        """Returns the number of iterations taken for the last solution. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(7), ctypes.c_int32(0)))
        return result

    def solution_read_maxiterations(self):
        """Returns the Maximum number of iterations used to solve the circuit. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result

    def solution_write_maxiterations(self, argument):
        """Modifies the Maximum number of iterations used to solve the circuit. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(9), ctypes.c_int32(argument)))
        return result

    def solution_read_number(self):
        """Returns the number of solutions to perform for MonteCarlo and time series simulations. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result

    def solution_write_number(self, argument):
        """Modifies the number of solutions to perform for MonteCarlo and time series simulations. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(11), ctypes.c_int32(argument)))
        return result

    def solution_read_random(self):
        """Returns the randomization mode for random variables "Gaussian" o "Uniform". """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(12), ctypes.c_int32(0)))
        return result

    def solution_write_random(self, argument):
        """Modifies the randomization mode for random variables "Gaussian" o "Uniform". """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(13), ctypes.c_int32(argument)))
        return result

    def solution_read_loadmodel(self):
        """Returns the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(14), ctypes.c_int32(0)))
        return result

    def solution_write_loadmodel(self, argument):
        """Modifies the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(15), ctypes.c_int32(argument)))
        return result

    def solution_read_addtype(self):
        """Returns the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(16), ctypes.c_int32(0)))
        return result

    def solution_write_addtype(self, argument):
        """Modifies the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(17), ctypes.c_int32(argument)))
        return result

    def solution_read_algorithm(self):
        """Returns the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(18), ctypes.c_int32(0)))
        return result

    def solution_write_algorithm(self, argument):
        """Modifies the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(19), ctypes.c_int32(argument)))
        return result

    def solution_read_controlmode(self):
        """Returns the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(20), ctypes.c_int32(0)))
        return result

    def solution_write_controlmode(self, argument):
        """Modifies the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(21), ctypes.c_int32(argument)))
        return result

    def solution_read_controliterations(self):
        """Returns the current value of the control iteration counter. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(22), ctypes.c_int32(0)))
        return result

    def solution_write_controliterations(self, argument):
        """Modifies the current value of the control iteration counter. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(23), ctypes.c_int32(argument)))
        return result

    def solution_read_maxcontroliterations(self):
        """Returns the maximum allowable control iterations."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(24), ctypes.c_int32(0)))
        return result

    def solution_write_maxcontroliterations(self, argument):
        """Modifies the maximum allowable control iterations. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(25), ctypes.c_int32(argument)))
        return result

    def solution_sampledocontrolactions(self):
        """Sample controls and then process the control queue for present control mode and dispatch control actions.
        Returns 0."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(26), ctypes.c_int32(0)))
        return result

    def solution_checkfaultstatus(self):
        """Executes status check on all fault objects defined in the circuit. Returns 0."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(27), ctypes.c_int32(0)))
        return result

    def solution_solvedirect(self):
        """Executes a direct solution from the system Y matrix, ignoring compensation currents of loads, generators
        (includes Yprim only)."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(28), ctypes.c_int32(0)))
        return result

    def solution_solvepflow(self):
        """Solves using present power flow method. Iterative solution rather than direct solution. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(29), ctypes.c_int32(0)))
        return result

    def solution_solvenocontrol(self):
        """Is similar to SolveSnap except no control actions are checked or executed."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(30), ctypes.c_int32(0)))
        return result

    def solution_solvepluscontrol(self):
        """Executes a power flow solution (SolveNoControl) plus executes a CheckControlActions that executes any
        pending control actions."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(31), ctypes.c_int32(0)))
        return result

    def solution_initsnap(self):
        """Initializes some variables for snap shot power flow. SolveSnap does this automatically."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(32), ctypes.c_int32(0)))
        return result

    def solution_checkcontrols(self):
        """Performs the normal process for sampling and executing Control Actions and Fault Status and rebuilds Y
        if necessary."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(33), ctypes.c_int32(0)))
        return result

    def solution_samplecontroldevices(self):
        """Executes a sampling of all intrinsic control devices, which push control actions into the control queue."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(34), ctypes.c_int32(0)))
        return result

    def solution_docontrolactions(self):
        """Pops control actions off the control queue and dispatches to the proper control element."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(35), ctypes.c_int32(0)))
        return result

    def solution_buildymatrix(self):
        """Forces building of the System Y matrix according to the argument:
        {1= series elements only | 2= Whole Y matrix}."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(36), ctypes.c_int32(0)))
        return result

    def solution_systemychanged(self):
        """Indicates if elements of the System Y have been changed by recent activity. If changed returns 1;
        otherwise 0."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(37), ctypes.c_int32(0)))
        return result

    def solution_read_converged(self):
        """Indicates whether the circuit solution converged (1 converged | 0 not converged)."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(38), ctypes.c_int32(0)))
        return result

    def solution_write_converged(self, argument):
        """Modifies the converged flag (1 converged | 0 not converged). """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(39), ctypes.c_int32(argument)))
        return result

    def solution_totaliterations(self):
        """Returns the total iterations including control iterations for most recent solution."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(40), ctypes.c_int32(0)))
        return result

    def solution_mostiterationsdone(self):
        """Returns the max number of iterations required to converge at any control iteration of the most recent
        solution.."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(41), ctypes.c_int32(0)))
        return result

    def solution_read_controlactionsdone(self):
        """Indicates that the control actions are done: {1 done, 0 not done}."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(42), ctypes.c_int32(0)))
        return result

    def solution_write_controlactionsdone(self, argument):
        """Modifies the flag to indicate that the control actions are done: {1 done, 0 not done}. """
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(43), ctypes.c_int32(argument)))
        return result

    def solution_finishtimestep(self):
        """Calls cleanup, sample monitors, and increment time at end of time step."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(44), ctypes.c_int32(0)))
        return result

    def solution_cleanup(self):
        """Update storage, invcontrol, etc., at end of time step."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(45), ctypes.c_int32(0)))
        return result

    def solution_solveall(self):
        """Starts the solution process for all the actors created in memory.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
        command."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(46), ctypes.c_int32(0)))
        return result

    def solution_calcincmatrix(self):
        """Starts the calculation of the incidence matrix for the active actor.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
        command."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(47), ctypes.c_int32(0)))
        return result

    def solution_calcincmatrix_0(self):
        """Starts the calculation of the Branch to Node incidence matrix for the active actor.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before
        using this command. The difference between this command and the CalcIncMatrix is that the calculated matrix
        will be ordered hierarchically from the substation to the feeder end, which can be helpful for many operations.
        Additionally, the Bus Levels vector is calculated and the rows (PDElements) and columns (Buses) are permuted so
        it is easy to identify their position in the circuit."""
        result = int(self.dss_obj.SolutionI(ctypes.c_int32(48), ctypes.c_int32(0)))
        return result

    # SolutionF (Float)
    def solution_read_frequency(self):
        """Returns the frequency for the next solution."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def solution_write_frequency(self, argument):
        """Sets the frequency for the next solution."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def solution_read_seconds(self):
        """Returns the seconds from top of the hour."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def solution_write_seconds(self, argument):
        """Sets the seconds from top of the hour."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def solution_read_stepsize(self):
        """Returns the step size for the next solution."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def solution_write_stepsize(self, argument):
        """Sets the step size for the next solution."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def solution_read_loadmult(self):
        """Returns the default load multiplier applied to all non-fixed loads."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def solution_write_loadmult(self, argument):
        """Sets the default load multiplier applied to all non-fixed loads."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def solution_read_tolerance(self):
        """Returns the solution convergence tolerance."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def solution_write_tolerance(self, argument):
        """Sets the solution convergence tolerance."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def solution_read_pctgrowth(self):
        """Returns the percent default annual load growth rate."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def solution_write_pctgrowth(self, argument):
        """Sets the percent default annual load growth rate."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def solution_read_genkw(self):
        """Returns the generator kW for AutoAdd mode."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def solution_write_genkw(self, argument):
        """Sets the generator kW for AutoAdd mode."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def solution_read_genpf(self):
        """Returns the pf for generators in AutoAdd mode."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def solution_write_genpf(self, argument):
        """Sets the pf for generators in AutoAdd mode."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def solution_read_capkvar(self):
        """Returns the capacitor kvar for adding in AutoAdd mode."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def solution_write_capkvar(self, argument):
        """Sets the capacitor kvar for adding in AutoAdd mode."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def solution_read_genmult(self):
        """Returns the default multiplier applied to generators (like LoadMult)."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def solution_write_genmult(self, argument):
        """Sets the default multiplier applied to generators (like LoadMult)."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def solution_read_dblhour(self):
        """Returns the hour as a double, including fractional part."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def solution_write_dblhour(self, argument):
        """Sets the hour as a double, including fractional part."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def solution_stepsizemin(self):
        """Sets the step size in minutes."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def solution_stepsizehr(self):
        """Sets the step size in Hours."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(23), ctypes.c_double(0)))
        return result

    def solution_processtime(self):
        """Retrieves the time required (microseconds) to perform the latest solution time step,
        this time does not include the time required for sampling meters/monitors."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    def solution_read_totaltime(self):
        """Retrieves the accumulated time required (microseconds) to perform the simulation."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(25), ctypes.c_double(0)))
        return result

    def solution_write_totaltime(self, argument):
        """Sets the accumulated time (microseconds) register.
        The new value for this register must be specified in the argument."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(26), ctypes.c_double(argument)))
        return result

    def solution_processtimetimestep(self):
        """Retrieves the time required (microseconds) to perform the latest solution time step including the time
        required for sampling meters/monitors."""
        result = float(self.dss_obj.SolutionF(ctypes.c_int32(27), ctypes.c_double(0)))
        return result

    # SolutionS (String)
    def solution_modeid(self):
        """Returns the ID (text) of the present solution mode."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_read_ldcurve(self):
        """Returns the Load-Duration Curve name for LD modes."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_ldcurve(self, argument):
        """Sets the Load-Duration Curve name for LD modes."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def solution_read_defaultdaily(self):
        """Returns the default daily load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_defaultdaily(self, argument):
        """Sets the default daily load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def solution_read_defaultyearly(self):
        """Returns the default yearly load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_defaultyearly(self, argument):
        """Sets the default yearly load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dss_obj.SolutionS(ctypes.c_int32(6), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SolutionV (Variant)
    def solution_eventlog(self):
        """Returns an array of strings containing the Event Log."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SolutionV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def solution_ncmatrix(self):
        """Returns an array of integers containing the incidence matrix (1-D).
        Each cell of the incidence matrix is delivered using 3 elements of the array delivered,
        the first is the row, the second is the column and the third is the value (1/-1).
        This procedure will only deliver the non-zero elements.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SolutionV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def solution_buslevels(self):
        """Returns an array of integers containing BusLevels array.
        This array gives a numeric value to each bus to specify how far it is from the
        circuit?s backbone (a continuous path from the feeder head to the feeder end).
        It is very handy to understand the circuit?s topology."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SolutionV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def solution_incmatrixrows(self):
        """Returns an array of strings specifying the way the rows of the incidence matrix (PDElements) are organized,
         depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
         the result could be very different.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SolutionV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def solution_incmatrixcols(self):
        """Returns an array of strings specifying the way the cols of the incidence matrix (buses) are organized,
        depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
        the result could be very different."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SolutionV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def solution_laplacian(self):
        """Returns an array of integers containing the Laplacian matrix using the incidence matrix previously calculated
        , this means that before calling this command the incidence matrix needs to be calculated using
        calcincmatrix/calcincmatrix_o. This command will return only the non-zero values in compressed coordinate
        format (row, col, value).."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SolutionV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value
