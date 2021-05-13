# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Parallel(Base):
    """
    These interfaces allows users to use the parallel processing features included in OpenDSS-PM. With this interface
    it is possible to create multiple actors, specify the CPU where the actor will be executed, control the execution
    of the actors, check the actors status and progress among many other functionalities.
    """

    # ParallelV (Variant)
    def parallel_actorprogress(self):
        """Returns an array of integers containing the progress in percentage for each active actor."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParallelV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def parallel_actorstatus(self):
        """Returns an array of integers containing the status of each active actor. If 1, the actor is ready to receive
         new commands, if 0, the actor is busy performing a simulation and cannot take new ?solve? commands at this time.
         However, the actor is capable to deliver values while the simulation is being performed."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.ParallelV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value
