# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class ParallelV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ParallelV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def parallel_actor_progress(self):
        """Returns an array of integers containing the progress in percentage for each active actor."""
        return Bridge.var_array_function(self.dss_obj.ParallelV, ctypes.c_int(0), ctypes.c_int(0), None)

    def parallel_actor_status(self):
        """Returns an array of integers containing the status of each active actor. If 1, the actor is ready to
        receive new commands, if 0, the actor is busy performing a simulation and cannot take new ?solve? commands at
        this time. However, the actor is capable to deliver values while the simulation is being performed. """
        return Bridge.var_array_function(self.dss_obj.ParallelV, ctypes.c_int(1), ctypes.c_int(0), None)
