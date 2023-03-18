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

    def _num_cpus(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(0), ctypes.c_int32(0))

    def _num_cores(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _active_actor_read(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _active_actor_write(self, argument: int) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def _create_actor(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(4), ctypes.c_int32(0))

    def _actor_cpu_read(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _actor_cpu_write(self, argument: int) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(6), ctypes.c_int32(argument))

    def _num_actors(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _wait(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(8), ctypes.c_int32(0))

    def _active_parallel_read(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _active_parallel_write(self, argument) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(10), ctypes.c_int32(argument))

    def _concatenate_reportsl_read(self) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(11), ctypes.c_int32(0))

    def _concatenate_reportsl_write(self, argument) -> int:
        return self._dss_obj.ParallelI(ctypes.c_int32(12), ctypes.c_int32(argument))
