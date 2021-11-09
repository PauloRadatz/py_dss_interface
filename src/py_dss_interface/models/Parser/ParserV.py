# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class ParserV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ParserV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    # TODO include in test
    def parser_vector(self):
        """Returns token as variant array of doubles. For parsing quoted array syntax."""
        return Bridge.var_array_function(self.dss_obj.ParserV, ctypes.c_int(0), ctypes.c_int(0), None)

    # TODO include in test
    def parser_matrix(self):
        """Use this property to parse a Matrix token in OpenDSS format. Returns square matrix of order specified.
        Order same as default fortran order: column by column. """
        return Bridge.var_array_function(self.dss_obj.ParserV, ctypes.c_int(1), ctypes.c_int(0), None)

    # TODO include in test
    def parser_sym_matrix(self):
        """Use this property to parse a Matrix token in lower triangular form. Symmetry is forced."""
        return Bridge.var_array_function(self.dss_obj.ParserV, ctypes.c_int(2), ctypes.c_int(0), None)
