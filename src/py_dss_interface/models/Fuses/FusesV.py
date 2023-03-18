# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.utils.Error import Error

class FusesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void FusesV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.FusesV, 0)

    def _state(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.FusesV, 1)

    def _state_write(self, argument: List[str]) -> List[str]:
        # return Bridge.variant_pointer_write(self.dss_obj.FusesV, 2, argument)
        Error.method_not_working("setter of dss.fuses.state")

    # TODO include in test
    def _normal(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.FusesV, 3)
        # Error.method_not_working("getter of dss.fuses.normal")

    # TODO include in test
    def _normal_write(self, argument: List[str]) -> List[str]:
        # return Bridge.variant_pointer_write(self.dss_obj.FusesV, 4, argument)
        Error.method_not_working("setter of dss.fuses.normal")
