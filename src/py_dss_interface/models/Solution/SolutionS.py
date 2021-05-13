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
