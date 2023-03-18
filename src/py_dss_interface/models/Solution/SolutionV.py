# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from typing import List
from py_dss_interface.utils.Error import Error


class SolutionV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SolutionV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _event_log(self) -> List[str]:
        Error.method_not_working("getter of dss.solution.event_log")
        # return Bridge.variant_pointer_read(self.dss_obj.SolutionV, 0)

    def _nc_matrix(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.SolutionV, 1)

    def _bus_levels(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.SolutionV, 2)

    def _inc_matrix_rows(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.SolutionV, 3)

    def _inc_matrix_cols(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.SolutionV, 4)

    def _laplacian(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.SolutionV, 5)
