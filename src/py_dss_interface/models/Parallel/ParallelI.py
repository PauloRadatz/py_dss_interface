# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Parallel(Base):

    # ParallelI (int)
    def parallel_numcpus(self):
        """Returns the number of CPUs available in the local computer."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def parallel_numcores(self):
        """Returns the number of physical cores available in the local computer.
        If your computer has less than 64 Cores, this number should be the number of CPUs/2.
        For more information,
        please check: https://www.howtogeek.com/194756/cpu-basics-multiple-cpus-cores-and-hyper-threading-explained/."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def parallel_read_activeactor(self):
        """Returns the ID of the active actor."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def parallel_write_activeactor(self, argument):
        """Sets the ID of the active actor; this number cannot be higher than the number of existing actors."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def parallel_createactor(self):
        """Creates a new actor and sets the active actor ID as the ID for the recently created actor.
        If there are no more CPUs available, the system will not allow the creation of the new actor."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def parallel_read_actorcpu(self):
        """Gets the ID of the CPU assigned for the execution of the active actor."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def parallel_write_actorcpu(self, argument):
        """Sets the CPU for the execution of the active actor."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def parallel_numactors(self):
        """Gets the number of actors created in the actual session."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def parallel_wait(self):
        """Waits until all the actors are free and ready to receive a new command."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def parallel_read_activeparallel(self):
        """Gets if the parallel features of OpenDSS-PM are active. If active, this parameter will return 1, otherwise,
         will return 0 and OpenDSS-PM will behave sequentially."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def parallel_write_activeparallel(self, argument):
        """Sets enables/disables the parallel features of OpenDSS-PM. To enable set the argument in 1, otherwise,
        the argument should be 0 and OpenDSS-PM will behave sequentially."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    def parallel_read_concatenatereportsl(self):
        """Gets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
         executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor
         will be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
         activate the actor of interest and then perform the Show/Export command on the desired monitor."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def parallel_write_concatenatereportsl(self, argument):
        """Sets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
        executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor will
         be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
         activate the actor of interest and then perform the Show/Export command on the desired monitor."""
        result = self.dss_obj.ParallelI(ctypes.c_int32(12), ctypes.c_int32(argument))
        return result

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
