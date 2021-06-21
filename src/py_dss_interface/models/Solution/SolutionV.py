# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class SolutionV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SolutionV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def solution_event_log(self):
        """Returns an array of strings containing the Event Log."""
        return Bridge.var_array_function(self.dss_obj.SolutionV, ctypes.c_int(0), ctypes.c_int(0), None)

    def solution_nc_matrix(self):
        """Returns an array of integers containing the incidence matrix (1-D). Each cell of the incidence matrix is
        delivered using 3 elements of the array delivered, the first is the row, the second is the column and the
        third is the value (1/-1). This procedure will only deliver the non-zero elements.. """
        return Bridge.var_array_function(self.dss_obj.SolutionV, ctypes.c_int(1), ctypes.c_int(0), None)

    def solution_bus_levels(self):
        """Returns an array of integers containing BusLevels array. This array gives a numeric value to each bus to
        specify how far it is from the circuit?s backbone (a continuous path from the feeder head to the feeder end).
        It is very handy to understand the circuit?s topology. """
        return Bridge.var_array_function(self.dss_obj.SolutionV, ctypes.c_int(2), ctypes.c_int(0), None)

    def solution_inc_matrix_rows(self):
        """Returns an array of strings specifying the way the rows of the incidence matrix (PDElements) are organized,
         depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
         the result could be very different.."""
        return Bridge.var_array_function(self.dss_obj.SolutionV, ctypes.c_int(3), ctypes.c_int(0), None)

    def solution_inc_matrix_cols(self):
        """Returns an array of strings specifying the way the cols of the incidence matrix (buses) are organized,
        depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
        the result could be very different."""
        return Bridge.var_array_function(self.dss_obj.SolutionV, ctypes.c_int(4), ctypes.c_int(0), None)

    def solution_laplacian(self):
        """Returns an array of integers containing the Laplacian matrix using the incidence matrix previously calculated
        , this means that before calling this command the incidence matrix needs to be calculated using
        calcincmatrix/calcincmatrix_o. This command will return only the non-zero values in compressed coordinate
        format (row, col, value).."""
        return Bridge.var_array_function(self.dss_obj.SolutionV, ctypes.c_int(5), ctypes.c_int(0), None)
