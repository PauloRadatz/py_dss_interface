# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class CircuitV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CircuitV(int32_t Parameter, VARIANT *Argument, int32_t Argument2);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def circuit_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total losses of the
        active circuit. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 0, None, '')

    def circuit_line_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total Line losses of
        the active circuit. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 1, None, '')

    def circuit_substation_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total transformer
        losses of the active circuit. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 2, None, '')

    def circuit_total_power(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total power in watts
        delivered to the active circuit. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 3, None, '')

    def circuit_all_bus_volts(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the node voltages from
        the most recent solution. Argument2 must be 0. """
        return Bridge.var_array_function(self.dss_obj.CircuitV, 4, None, '')

    def circuit_all_bus_vmag(self) -> List[float]:
        """Returns an array of doubles (magnitude) with the node voltages from the most recent solution. Argument2
        must be 0. """
        return Bridge.var_array_function(self.dss_obj.CircuitV, 5, None, '')

    def circuit_all_element_names(self) -> List[str]:
        """Returns an array of strings with the names of all the elements of the active circuit. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 6, None, '')

    def circuit_all_bus_names(self) -> List[str]:
        """Returns an array of strings with the names of all the Buses of the active circuit (See AllNodeNames).
        Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 7, None, '')

    def circuit_all_element_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the losses in each element
        of the active circuit. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 8, None, '')

    def circuit_all_bus_vmag_pu(self) -> List[float]:
        """Returns an array of doubles with the voltages in per unit of the most recent solution of the active circuit.
        Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 9, None, '')

    def circuit_all_node_names(self) -> List[str]:
        """Returns an array of strings containing full name of each node in system in same order as returned by
        AllBusVolts, etc. Argument2 must be 0. """
        return Bridge.var_array_function(self.dss_obj.CircuitV, 10, None, '')

    def circuit_system_y(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) containing the Y Bus Matrix of
        the system (after a solution has been performed).
        Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 11, None, '')

    def circuit_all_bus_distances(self) -> List[float]:
        """Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames. Argument2
        must be 0. """
        return Bridge.var_array_function(self.dss_obj.CircuitV, 12, None, '')

    def circuit_all_node_distances(self) -> List[float]:
        """Returns distance from each Node to parent EnergyMeter. Corresponds to sequence in AllBusVmag. Argument2
        must be 0. """
        return Bridge.var_array_function(self.dss_obj.CircuitV, 13, None, '')

    def circuit_all_node_vmag_by_phase(self, argument: int = 1) -> List[float]:
        """Returns array of doubles representing the voltage magnitudes for nodes on the specified phase. The phase
        must be specified in the Argument2. """
        argument = Base.check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.var_array_function(self.dss_obj.CircuitV, 14, argument, '')

    def circuit_all_node_vmag_pu_by_phase(self, argument: int = 1) -> List[float]:
        """Returns array of doubles representing the voltage magnitudes (in per unit) for nodes on the specified phase.
        The phase must be specified in the Argument2."""
        argument = Base.check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.var_array_function(self.dss_obj.CircuitV, 15, argument, '')

    def circuit_all_node_distances_by_phase(self, argument: int = 1) -> List[float]:
        """Returns array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to
        other node ByPhase properties. Argument2 must contain the number of the phase to return."""
        argument = Base.check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.var_array_function(self.dss_obj.CircuitV, 16, argument, '')

    def circuit_all_node_names_by_phase(self, argument: int = 1) -> List[str]:
        """Returns array of strings of the node names by Phase criteria. Sequence corresponds to other ByPhase
        properties. Argument2 must contain the number of the phase to return. """
        argument = Base.check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.var_array_function(self.dss_obj.CircuitV, 17, argument, '')

    def circuit_y_node_varray(self):
        """Returns a complex array of actual node voltages in same order as SystemY Matrix. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 18, None, '')

    def circuit_y_node_order(self) -> List[str]:
        """Returns a variant array of strings containing the names of the nodes in the same order as the Y Matrix.
        Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 19, None, '')

    def circuit_y_currents(self):
        """Returns a variant array of doubles containing complex injection currents for the present solution.
        It is the "I" vector of I=YV. Argument2 must be 0."""
        return Bridge.var_array_function(self.dss_obj.CircuitV, 20, None, '')
