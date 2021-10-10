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

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def circuit_integer(self, first: int, second: int) -> int:
        return int(self.dss_obj.CircuitI(ctypes.c_int32(first), ctypes.c_int32(second)))

    def circuit_num_ckt_elements(self) -> int:
        """Will deliver the number of CktElements included in the active circuit."""
        return self.circuit_integer(0, 0)

    def circuit_num_buses(self) -> int:
        """Will deliver the number of buses included in the active circuit."""
        return self.circuit_integer(1, 0)

    def circuit_num_nodes(self) -> int:
        """Will deliver the number of nodes included in the active circuit."""
        return self.circuit_integer(2, 0)

    def circuit_first_pc_element(self) -> int:
        """Sets the first PCElement to be the active PCElement, as a result, this parameter will deliver the index of
        the active PCElement (ideally 1). """
        return self.circuit_integer(3, 0)

    def circuit_next_pc_element(self) -> int:
        """Sets the next PCElement to be the active PCElement, as a result, this parameter will deliver the index of
        the active PCElement (if there is no more it will return a 0). """
        return self.circuit_integer(4, 0)

    def circuit_first_pd_element(self) -> int:
        """Sets the first PDElement to be the active PDElement, as a result, this parameter will deliver the index of
        the active PDElement (ideally 1). """
        return self.circuit_integer(5, 0)

    def circuit_next_pd_element(self) -> int:
        """Sets the next PDElement to be the active PDElement, as a result, this parameter will deliver the index of the
         active PDElement (if there is no more it will return a 0)."""
        return self.circuit_integer(6, 0)

    def circuit_sample(self) -> int:
        """Forces all meters and monitors to take a sample, returns 0."""
        return self.circuit_integer(7, 0)

    def circuit_save_sample(self) -> int:
        """Forces all meters and monitors to save their sample buffers, returns 0."""
        return self.circuit_integer(8, 0)

    def circuit_set_active_bus_i(self, i: int) -> int:
        """Sets active the bus specified by index, which is compatible with the index delivered by AllBusNames,
        returns 0 it everything ok."""
        i = Base.check_int_param(i, 1)
        return self.circuit_integer(9, i)

    def circuit_first_element(self) -> int:
        """Sets the first Element of the active class to be the active Element, as a result, this parameter will
        deliver the index of the active Element (0 if none). """
        return self.circuit_integer(10, 0)

    def circuit_next_element(self) -> int:
        """Sets the next Element of the active class to be the active Element, as a result, this parameter will
        deliver the index of the active Element (0 if none). """
        return self.circuit_integer(11, 0)

    def circuit_update_storage_t(self):
        """Forces all storage classes to update. Typically done after a solution."""
        return self.circuit_integer(12, 0)

    def circuit_parent_pd_element(self) -> int:
        """Sets parent PD Element, if any, to be the active circuit element and returns index > 0 if it fails or not
        applicable. """
        return self.circuit_integer(13, 0)

    def circuit_end_of_time_step_update(self) -> int:
        """Calls end of time step cleanup routine in solutionalgs.pas. Returns 0."""
        return self.circuit_integer(14, 0)
