# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class ParallelI(Base):
    """
    This interface allows to control parameters of the parallel computing suite of OpenDSS-PM where its value can be
    specified as an integer number.

    The structure of the interface is as follows:
        int32_t ParalleI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def parallel_num_cpus(self) -> int:
        """Returns the number of CPUs available in the local computer."""
        return self.dss_obj.ParallelI(ctypes.c_int32(0), ctypes.c_int32(0))

    def parallel_num_cores(self) -> int:
        """Returns the number of physical cores available in the local computer. If your computer has less than 64
        Cores, this number should be the number of CPUs/2. For more information, please check:
        https://www.howtogeek.com/194756/cpu-basics-multiple-cpus-cores-and-hyper-threading-explained/. """
        return self.dss_obj.ParallelI(ctypes.c_int32(1), ctypes.c_int32(0))

    def parallel_read_active_actor(self) -> int:
        """Returns the ID of the active actor."""
        return self.dss_obj.ParallelI(ctypes.c_int32(2), ctypes.c_int32(0))

    def parallel_write_active_actor(self, argument) -> int:
        """Sets the ID of the active actor; this number cannot be higher than the number of existing actors."""
        return self.dss_obj.ParallelI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def parallel_create_actor(self) -> int:
        """Creates a new actor and sets the active actor ID as the ID for the recently created actor. If there are no
        more CPUs available, the system will not allow the creation of the new actor. """
        return self.dss_obj.ParallelI(ctypes.c_int32(4), ctypes.c_int32(0))

    def parallel_read_actor_cpu(self) -> int:
        """Gets the ID of the CPU assigned for the execution of the active actor."""
        return self.dss_obj.ParallelI(ctypes.c_int32(5), ctypes.c_int32(0))

    def parallel_write_actor_cpu(self, argument) -> int:
        """Sets the CPU for the execution of the active actor."""
        return self.dss_obj.ParallelI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def parallel_num_actors(self) -> int:
        """Gets the number of actors created in the actual session."""
        return self.dss_obj.ParallelI(ctypes.c_int32(7), ctypes.c_int32(0))

    def parallel_wait(self) -> int:
        """Waits until all the actors are free and ready to receive a new command."""
        return self.dss_obj.ParallelI(ctypes.c_int32(8), ctypes.c_int32(0))

    def parallel_read_active_parallel(self) -> int:
        """Gets if the parallel features of OpenDSS-PM are active. If active, this parameter will return 1, otherwise,
         will return 0 and OpenDSS-PM will behave sequentially."""
        return self.dss_obj.ParallelI(ctypes.c_int32(9), ctypes.c_int32(0))

    def parallel_write_active_parallel(self, argument) -> int:
        """Sets enables/disables the parallel features of OpenDSS-PM. To enable set the argument in 1, otherwise,
        the argument should be 0 and OpenDSS-PM will behave sequentially. """
        return self.dss_obj.ParallelI(ctypes.c_int32(10), ctypes.c_int32(argument))

    def parallel_read_concatenate_reportsl(self) -> int:
        """Gets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
        executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor
        will be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
        activate the actor of interest and then perform the Show/Export command on the desired monitor. """
        return self.dss_obj.ParallelI(ctypes.c_int32(11), ctypes.c_int32(0))

    def parallel_write_concatenate_reportsl(self, argument) -> int:
        """Sets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
        executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor
        will be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
        activate the actor of interest and then perform the Show/Export command on the desired monitor. """
        return self.dss_obj.ParallelI(ctypes.c_int32(12), ctypes.c_int32(argument))
