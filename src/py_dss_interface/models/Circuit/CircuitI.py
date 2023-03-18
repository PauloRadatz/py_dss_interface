# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CircuitI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t CircuitI(int32_t Parameter, int32_t Argument);

    This interface returns an _integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    # TODO: Must be reviewed
    def _integer(self, first: int, second: int) -> int:
        return int(self._dss_obj.CircuitI(ctypes.c_int32(first), ctypes.c_int32(second)))

    def _num_ckt_elements(self) -> int:
        return self._integer(0, 0)

    def _num_buses(self) -> int:
        return self._integer(1, 0)

    def _num_nodes(self) -> int:
        return self._integer(2, 0)

    def _pc_element_first(self) -> int:
        return self._integer(3, 0)

    def _pc_element_next(self) -> int:
        return self._integer(4, 0)

    def _pd_element_first(self) -> int:
        return self._integer(5, 0)

    def _pd_element_next(self) -> int:
        return self._integer(6, 0)

    def _sample(self) -> int:

        return self._integer(7, 0)

    def _save_sample(self) -> int:
        return self._integer(8, 0)

    def _set_active_bus_i(self, i: int) -> int:
        i = Base._check_int_param(i, 1)
        return self._integer(9, i)

    def _first_element(self) -> int:
        return self._integer(10, 0)

    def _next_element(self) -> int:
        return self._integer(11, 0)

    def _update_storage_t(self):
        return self._integer(12, 0)

    def _parent_pd_element(self) -> int:
        return self._integer(13, 0)

    def _end_of_time_step_update(self) -> int:
        return self._integer(14, 0)
