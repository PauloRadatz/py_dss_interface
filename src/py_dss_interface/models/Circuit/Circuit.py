# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Circuit.CircuitF import CircuitF
from py_dss_interface.models.Circuit.CircuitI import CircuitI
from py_dss_interface.models.Circuit.CircuitS import CircuitS
from py_dss_interface.models.Circuit.CircuitV import CircuitV


class Circuit(CircuitI, CircuitS, CircuitF, CircuitV):
    """
    This interface implements the Circuit (ICIrcuit) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: CircuitI, CircuitS, CircuitF, CircuitV
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def capacity(self, capacity_start=0, capacity_increment=0.1) -> float:
        """Returns the total capacity of the active circuit. Or this parameter it is necessary to specify the start
                and increment of the capacity in the arguments argument1 and argument2 respectively. """
        return CircuitF._capacity(self, capacity_start, capacity_increment)

    @property
    def losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total losses of the
                active circuit. Argument2 must be 0."""
        return CircuitV._losses(self)

    @property
    def line_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total Line losses of
                the active circuit. Argument2 must be 0."""
        return CircuitV._line_losses(self)

    @property
    def substation_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total transformer
                losses of the active circuit. Argument2 must be 0."""
        return CircuitV._substation_losses(self)

    @property
    def total_power(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the total power in watts
                delivered to the active circuit. Argument2 must be 0."""
        return CircuitV._total_power(self)

    @property
    def buses_volts(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the node voltages from
                the most recent solution. Argument2 must be 0. """
        return CircuitV._buses_volts(self)

    @property
    def buses_vmag(self) -> List[float]:
        """Returns an array of doubles (magnitude) with the node voltages from the most recent solution. Argument2
                must be 0. """
        return CircuitV._buses_vmag(self)

    @property
    def elements_names(self) -> List[str]:
        """Returns an array of strings with the names of all the elements of the active circuit. Argument2 must be 0."""
        return CircuitV._elements_names(self)

    @property
    def buses_names(self) -> List[str]:
        """Returns an array of strings with the names of all the Buses of the active circuit (See AllNodeNames).
                Argument2 must be 0."""
        return CircuitV._buses_names(self)

    @property
    def elements_losses(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) with the losses in each element
                of the active circuit. Argument2 must be 0."""
        return CircuitV._elements_losses(self)

    @property
    def buses_vmag_pu(self) -> List[float]:
        """Returns an array of doubles with the voltages in per unit of the most recent solution of the active circuit.
                Argument2 must be 0."""
        return CircuitV._buses_vmag_pu(self)

    @property
    def nodes_names(self) -> List[str]:
        """Returns an array of strings containing full name of each node in system in same order as returned by
                AllBusVolts, etc. Argument2 must be 0. """
        return CircuitV._nodes_names(self)

    @property
    def system_y(self) -> List[float]:
        """Returns an array of doubles (two doubles for representing a complex number) containing the Y Bus Matrix of
                the system (after a solution has been performed).
                Argument2 must be 0."""
        return CircuitV._system_y(self)

    @property
    def buses_distances(self) -> List[float]:
        """Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames. Argument2
                must be 0. """
        return CircuitV._buses_distances(self)

    @property
    def nodes_distances(self) -> List[float]:
        """Returns distance from each Node to parent EnergyMeter. Corresponds to sequence in AllBusVmag. Argument2
                must be 0. """
        return CircuitV._nodes_distances(self)

    def nodes_vmag_by_phase(self, argument: int = 1) -> List[float]:
        """Returns array of doubles representing the voltage magnitudes for nodes on the specified phase. The phase
                must be specified in the Argument2. """
        return CircuitV._nodes_vmag_by_phase(self, argument)

    def nodes_vmag_pu_by_phase(self, argument: int = 1) -> List[float]:
        """Returns array of doubles representing the voltage magnitudes (in per unit) for nodes on the specified phase.
                The phase must be specified in the Argument2."""
        return CircuitV._nodes_vmag_pu_by_phase(self, argument)

    def nodes_distances_by_phase(self, argument: int = 1) -> List[float]:
        """Returns array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to
                other node ByPhase properties. Argument2 must contain the number of the phase to return."""

        return CircuitV._nodes_distances_by_phase(self, argument)

    def nodes_names_by_phase(self, argument: int = 1) -> List[str]:
        """Returns array of strings of the node names by Phase criteria. Sequence corresponds to other ByPhase
                properties. Argument2 must contain the number of the phase to return. """
        return CircuitV._nodes_names_by_phase(self, argument)

    @property
    def y_node_order(self) -> List[str]:
        """Returns a variant array of strings containing the names of the nodes in the same order as the Y Matrix.
                Argument2 must be 0."""
        return CircuitV._y_node_order(self)

    @property
    def y_currents(self) -> List[float]:
        """Returns a variant array of doubles containing complex injection currents for the present solution.
                It is the "I" vector of I=YV. Argument2 must be 0."""
        return CircuitV._y_currents(self)

    @property
    def y_node_varray(self) -> List[float]:
        """Returns a complex array of actual node voltages in same order as SystemY Matrix. Argument2 must be 0."""
        return CircuitV._y_node_varray(self)

    @property
    def name(self) -> str:
        """Returns the name of the active circuit."""
        return CircuitS._name(self)

    @property
    def disable(self) -> str:
        """Allows to disable an element of the active circuit, the element must be specified by name.
                As a result, this parameter will deliver the string ?Ok?."""
        return CircuitS._disable(self)

    @property
    def enable(self) -> str:
        """Allows to enable an element of the active circuit, the element must be specified by name.
                As a result, this parameter will deliver the string ?Ok?."""
        return CircuitS._enable(self)

    @property
    def num_ckt_elements(self) -> int:
        """Will deliver the number of CktElements included in the active circuit."""
        return CircuitI._num_ckt_elements(self)

    @property
    def num_buses(self) -> int:
        """Will deliver the number of buses included in the active circuit."""
        return CircuitI._num_buses(self)

    @property
    def num_nodes(self) -> int:
        """Will deliver the number of nodes included in the active circuit."""
        return CircuitI._num_nodes(self)

    def pc_element_first(self) -> int:
        """Sets the first PCElement to be the active PCElement, as a result, this parameter will deliver the index of
                the active PCElement (ideally 1). """
        return CircuitI._pc_element_first(self)

    def pc_element_next(self) -> int:
        """Sets the next PCElement to be the active PCElement, as a result, this parameter will deliver the index of
                the active PCElement (if there is no more it will return a 0). """
        return CircuitI._pc_element_next(self)

    def pd_element_first(self) -> int:
        """Sets the first PDElement to be the active PDElement, as a result, this parameter will deliver the index of
                the active PDElement (ideally 1). """
        return CircuitI._pd_element_first(self)

    def pd_element_next(self) -> int:
        """Sets the next PDElement to be the active PDElement, as a result, this parameter will deliver the index of the
                 active PDElement (if there is no more it will return a 0)."""
        return CircuitI._pd_element_next(self)

    def first_element(self) -> int:
        """Sets the first Element of the active class to be the active Element, as a result, this parameter will
                deliver the index of the active Element (0 if none). """
        return CircuitI._first_element(self)

    def next_element(self) -> int:
        """Sets the next Element of the active class to be the active Element, as a result, this parameter will
                deliver the index of the active Element (0 if none). """
        return CircuitI._next_element(self)

    @property
    def sample(self) -> int:
        """Forces all meters and monitors to take a sample, returns 0."""
        return CircuitI._sample(self)

    @property
    def save_sample(self) -> int:
        """Forces all meters and monitors to save their sample buffers, returns 0."""
        return CircuitI._save_sample(self)

    @property
    def update_storage_t(self):
        """Forces all storage classes to update. Typically, done after a solution."""
        return CircuitI._update_storage_t(self)

    @property
    def parent_pd_element(self) -> int:
        """Sets parent PD Element, if any, to be the active circuit element and returns index > 0 if it fails or not
                applicable. """
        return CircuitI._parent_pd_element(self)

    @property
    def end_of_time_step_update(self) -> int:
        """Calls end of time step cleanup routine in solutionalgs.pas. Returns 0."""
        return CircuitI._end_of_time_step_update(self)

    def set_active_bus(self, active_bus: str):
        """Allows to activate a bus of the active circuit, the bus must be specified by name.
                As a result, this parameter will deliver a string with the index of the active Bus."""
        return CircuitS._set_active_bus(self, active_bus)

    def set_active_bus_i(self, i: int) -> int:
        """Sets active the bus specified by index, which is compatible with the index delivered by AllBusNames,
                returns 0 it everything ok."""
        return CircuitI._set_active_bus_i(self, i)

    def set_active_element(self, active_element: str):
        """Allows to activate an element of the active circuit, the element must be specified by name.
                As a result, this parameter will deliver a string with the index of the active element."""
        return CircuitS._set_active_element(self, active_element)

    def set_active_class(self, argument: str) -> str:
        """Allows tto activate a Class of the active circuit, the Class must be specified by name.
                As a result, this parameter will deliver a string with the index of the active Class."""
        return CircuitS._set_active_class(self, argument)
