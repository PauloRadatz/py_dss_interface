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
