# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class Parser(Base):
    """
    This interface implements the CmathLib (ICmathLib) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface.
    """

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ParserV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def parser_vector(self):
        """Returns token as variant array of doubles. For parsing quoted array syntax."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParserV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def parser_matrix(self):
        """Use this property to parse a Matrix token in OpenDSS format. Returns square matrix of order specified.
        Order same as default fortran order: column by column."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParserV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def parser_symmatrix(self):
        """Use this property to parse a Matrix token in lower triangular form. Symmetry is forced."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParserV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
