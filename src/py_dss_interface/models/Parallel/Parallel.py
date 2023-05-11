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
        """Returns the number of CPUs available in the local computer."""
        return ParallelI._num_cpus(self)

    @property
    def num_cores(self) -> int:
        """Returns the number of physical cores available in the local computer. If your computer has less than 64
                Cores, this number should be the number of CPUs/2. For more information, please check:
                https://www.howtogeek.com/194756/cpu-basics-multiple-cpus-cores-and-hyper-threading-explained/. """
        return ParallelI._num_cores(self)

    @property
    def active_actor(self) -> int:
        """Returns the ID of the active actor.
        Sets the ID of the active actor; this number cannot be higher than the number of existing actors."""
        return ParallelI._active_actor_read(self)

    @active_actor.setter
    def active_actor(self, argument: int):
        ParallelI._active_actor_write(self, argument)

    def create_actor(self) -> int:
        """Creates a new actor and sets the active actor ID as the ID for the recently created actor. If there are no
                more CPUs available, the system will not allow the creation of the new actor. """
        return ParallelI._create_actor(self)

    @property
    def actor_cpu(self) -> int:
        """Gets the ID of the CPU assigned for the execution of the active actor.

        Sets the CPU for the execution of the active actor."""
        return ParallelI._actor_cpu_read(self)

    @actor_cpu.setter
    def actor_cpu(self, argument: int):
        ParallelI._actor_cpu_write(self, argument)

    @property
    def num_actors(self) -> int:
        """Gets the number of actors created in the actual session."""
        return ParallelI._num_actors(self)

    def wait(self) -> int:
        """Waits until all the actors are free and ready to receive a new command."""
        return ParallelI._wait(self)

    @property
    def active_parallel(self) -> int:
        """Gets if the parallel features of OpenDSS-PM are active. If active, this parameter will return 1, otherwise,
                 will return 0 and OpenDSS-PM will behave sequentially.

        Sets enables/disables the parallel features of OpenDSS-PM. To enable set the argument in 1, otherwise,
        the argument should be 0 and OpenDSS-PM will behave sequentially. """
        return ParallelI._active_parallel_read(self)

    @active_parallel.setter
    def active_parallel(self, argument: int):
        ParallelI._active_parallel_write(self, argument)

    @property
    def concatenate_reportsl(self) -> int:
        """Gets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
                executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor
                will be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
                activate the actor of interest and then perform the Show/Export command on the desired monitor.

        Sets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
        executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor
        will be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
        activate the actor of interest and then perform the Show/Export command on the desired monitor. """
        return ParallelI._concatenate_reportsl_read(self)

    @concatenate_reportsl.setter
    def concatenate_reportsl(self, argument: int):
        ParallelI._concatenate_reportsl_write(self, argument)

    @property
    def actor_progress(self) -> List[int]:
        """Returns an array of integers containing the progress in percentage for each active actor."""
        return ParallelV._actor_progress(self)

    @property
    def actor_status(self) -> List[int]:
        """Returns an array of integers containing the status of each active actor. If 1, the actor is ready to
                receive new commands, if 0, the actor is busy performing a simulation and cannot take new ?solve? commands at
                this time. However, the actor is capable to deliver values while the simulation is being performed. """
        return ParallelV._actor_status(self)
