# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CircuitI(Base):

    def circuit_integer(self, first, second):
        return int(self.dss_obj.CircuitI(ctypes.c_int32(first), ctypes.c_int32(second)))

    def num_ckt_elements(self):
        """Will deliver the number of CktElements included in the active circuit."""
        return self.circuit_integer(0, 0)

    def num_buses(self):
        """Will deliver the number of buses included in the active circuit."""
        return self.circuit_integer(1, 0)

    def num_nodes(self):
        """Will deliver the number of nodes included in the active circuit."""
        return self.circuit_integer(2, 0)

    def first_pc_element(self):
        """Sets the first PCElement to be the active PCElement, as a result,
        this parameter will deliver the index of the active PCElement (ideally 1)."""
        return self.circuit_integer(3, 0)

    def next_pc_element(self):
        """Sets the next PCElement to be the active PCElement, as a result,
        this parameter will deliver the index of the active PCElement (if there is no more it will return a 0)."""
        return self.circuit_integer(4, 0)

    def first_pd_element(self):
        """Sets the first PDElement to be the active PDElement, as a result,
        this parameter will deliver the index of the active PDElement (ideally 1)."""
        return self.circuit_integer(5, 0)

    def next_pd_element(self):
        """Sets the next PDElement to be the active PDElement, as a result, this parameter will deliver the index of the
         active PDElement (if there is no more it will return a 0)."""
        return self.circuit_integer(6, 0)

    def sample(self):
        """Forces all meters and monitors to take a sample, returns 0."""
        return self.circuit_integer(7, 0)

    def save_sample(self):
        """Forces all meters and monitors to save their sample buffers, returns 0."""
        return self.circuit_integer(8, 0)

    def set_active_bus_i(self, i):
        """Sets active the bus specified by index, which is compatible with the index delivered by AllBusNames,
        returns 0 it everything ok."""
        return self.circuit_integer(9, 0)

    def first_element(self):
        """Sets the first Element of the active class to be the active Element, as a result,
        this parameter will deliver the index of the active Element (0 if none)."""
        return self.circuit_integer(10, 0)

    def next_element(self):
        """Sets the next Element of the active class to be the active Element, as a result,
        this parameter will deliver the index of the active Element (0 if none)."""
        return self.circuit_integer(11, 0)

    def update_storage_t(self):
        """Forces all storage classes to update. Typically done after a solution."""
        return self.circuit_integer(12, 0)

    def parent_pd_element(self):
        """Sets parent PD Element, if any, to be the active circuit element and returns
        index > 0 if it fails or not applicable."""
        return self.circuit_integer(13, 0)

    def end_of_time_step_update(self):
        """Calls end of time step cleanup routine in solutionalgs.pas. Returns 0."""
        return self.circuit_integer(14, 0)
