# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class ISourcesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void IsourcesV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    # TODO include in test
    def isources_all_names(self) -> List[str]:
        """Gets the variant array of string containing names of all ISources in the circuit."""
        return Bridge.var_array_function(self.dss_obj.IsourceV, 0, None, '')
