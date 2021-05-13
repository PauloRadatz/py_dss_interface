# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class ParallelV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void ParallelV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def parallel_actorprogress(self):
        """Returns an array of integers containing the progress in percentage for each active actor."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParallelV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def parallel_actorstatus(self):
        """Returns an array of integers containing the status of each active actor. If 1, the actor is ready to
        receive new commands, if 0, the actor is busy performing a simulation and cannot take new ?solve? commands at
        this time. However, the actor is capable to deliver values while the simulation is being performed. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParallelV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value
