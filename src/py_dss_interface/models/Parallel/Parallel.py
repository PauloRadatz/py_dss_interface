# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Parallel.ParallelI import ParallelI
from py_dss_interface.models.Parallel.ParallelV import ParallelV
from typing import List

class Parallel(ParallelI, ParallelV):
    """
    These interfaces allows users to use the parallel processing features included in OpenDSS-PM. With this interface
    it is possible to create multiple actors, specify the CPU where the actor will be executed, control the execution
    of the actors, check the actors status and progress among many other functionalities.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def num_cpus(self) -> int:
        return ParallelI._num_cpus(self)

    @property
    def num_cores(self) -> int:
        return ParallelI._num_cores(self)

    @property
    def active_actor(self) -> int:
        return ParallelI._active_actor_read(self)

    @active_actor.setter
    def active_actor(self, argument: int):
        ParallelI._active_actor_write(self, argument)

    def create_actor(self) -> int:
        return ParallelI._create_actor(self)

    @property
    def actor_cpu(self) -> int:
        return ParallelI._actor_cpu_read(self)

    @actor_cpu.setter
    def actor_cpu(self, argument: int):
        ParallelI._actor_cpu_write(self, argument)

    @property
    def num_actors(self) -> int:
        return ParallelI._num_actors(self)

    def wait(self) -> int:
        return ParallelI._wait(self)

    @property
    def active_parallel(self) -> int:
        return ParallelI._active_parallel_read(self)

    @active_parallel.setter
    def active_parallel(self, argument: int):
        ParallelI._active_parallel_write(self, argument)

    @property
    def concatenate_reportsl(self) -> int:
        return ParallelI._concatenate_reportsl_read(self)

    @concatenate_reportsl.setter
    def concatenate_reportsl(self, argument: int):
        ParallelI._concatenate_reportsl_write(self, argument)

    @property
    def actor_progress(self) -> List[int]:
        return ParallelV._actor_progress(self)

    @property
    def actor_status(self) -> List[int]:
        return ParallelV._actor_status(self)
