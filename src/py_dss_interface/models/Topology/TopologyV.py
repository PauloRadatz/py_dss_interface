# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class TopologyV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void TopologyV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def topology_all_looped_pairs(self):
        """Gets a variant array of all looped element names, by pairs."""
        return Bridge.var_array_function(self.dss_obj.TopologyV, ctypes.c_int(0), ctypes.c_int(0), None)

    def topology_all_isolated_branches(self):
        """Gets a variant array of all isolated branch names."""
        return Bridge.var_array_function(self.dss_obj.TopologyV, ctypes.c_int(1), ctypes.c_int(0), None)

    def topology_all_isolated_loads(self):
        """Gets a variant array of all isolated load names."""
        return Bridge.var_array_function(self.dss_obj.TopologyV, ctypes.c_int(2), ctypes.c_int(0), None)
