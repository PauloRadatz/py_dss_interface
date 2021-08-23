# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class FusesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void FusesV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def fuses_all_names(self) -> List[str]:
        """Gets the variant array of string containing names of all fuses in the circuit."""
        return Bridge.var_array_function(self.dss_obj.FusesV, 0, None, '')

    def fuses_read_state(self):
        """Gets a variant array of strings[0..Nphases-1] indicating the present state for all phases of the active fuse.
        If value is -1 an error has occurred."""
        return Bridge.var_array_function(self.dss_obj.FusesV, 1, None, '')

    def fuses_write_state(self, argument):
        """Sets a variant array of strings [0..Nphases-1] indicating the state for all phases of the active fuse.
        If value is -1 an error has occurred."""
        argument = Base.check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.var_array_function(self.dss_obj.FusesV, 2, argument, '')

    def fuses_read_normal(self):
        """Gets a variant array of strings[0..Nphases-1] indicating the normal state for all phases of the active fuse.
        If value is -1 an error has occurred."""
        return Bridge.var_array_function(self.dss_obj.FusesV, 3, None, '')

    def fuses_write_normal(self, argument):
        """Sets a variant array of strings [0..Nphases-1] indicating the state for all phases of the active fuse.
        If value is -1 an error has occurred."""
        argument = Base.check_int_param(argument, default=1)
        return Bridge.var_array_function(self.dss_obj.FusesV, 4, argument, '')
