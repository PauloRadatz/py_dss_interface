# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Parallel.ParallelI import ParallelI
from py_dss_interface.models.Parallel.ParallelV import ParallelV


class Parallel(ParallelI, ParallelV):
    """
    These interfaces allows users to use the parallel processing features included in OpenDSS-PM. With this interface
    it is possible to create multiple actors, specify the CPU where the actor will be executed, control the execution
    of the actors, check the actors status and progress among many other functionalities.
    """
    pass
