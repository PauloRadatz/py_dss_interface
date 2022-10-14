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
    def circuit_float(self, first, second, third) -> float:
        return CircuitF._circuit_float(self, first, second, third)

    @property
    def capacity(self, capacity_start=0, capacity_increment=0.1) -> float:
        return CircuitF._capacity(self, capacity_start, capacity_increment)

    @property
    def losses(self) -> List[float]:
        return CircuitV._losses(self)

    @property
    def line_losses(self) -> List[float]:
        return CircuitV._line_losses(self)

    @property
    def substation_losses(self) -> List[float]:
        return CircuitV._substation_losses(self)

    @property
    def total_power(self) -> List[float]:
        return CircuitV._total_power(self)

    @property
    def buses_volts(self) -> List[float]:
        return CircuitV._buses_volts(self)

    @property
    def buses_vmag(self) -> List[float]:
        return CircuitV._buses_vmag(self)

    @property
    def elements_names(self) -> List[str]:
        return CircuitV._elements_names(self)

    @property
    def buses_names(self) -> List[str]:
        return CircuitV._buses_names(self)

    @property
    def elements_losses(self) -> List[float]:
        return CircuitV._elements_losses(self)

    @property
    def buses_vmag_pu(self) -> List[float]:
        return CircuitV._buses_vmag_pu(self)

    @property
    def nodes_names(self) -> List[str]:
        return CircuitV._nodes_names(self)

    @property
    def system_y(self) -> List[float]:
        return CircuitV._system_y(self)

    @property
    def buses_distances(self) -> List[float]:
        return CircuitV._buses_distances(self)

    @property
    def nodes_distances(self) -> List[float]:
        return CircuitV._nodes_distances(self)

    def nodes_vmag_by_phase(self, argument: int = 1) -> List[float]:
        return CircuitV._nodes_vmag_by_phase(self, argument)

    def nodes_vmag_pu_by_phase(self, argument: int = 1) -> List[float]:
        return CircuitV._nodes_vmag_pu_by_phase(self, argument)

    def nodes_distances_by_phase(self, argument: int = 1) -> List[float]:
        return CircuitV._nodes_distances_by_phase(self, argument)

    def nodes_names_by_phase(self, argument: int = 1) -> List[str]:
        return CircuitV._nodes_names_by_phase(self, argument)

    @property
    def y_node_order(self) -> List[str]:
        return CircuitV._y_node_order(self)

    @property
    def y_currents(self):
        return CircuitV._y_currents(self)

    @property
    def y_node_varray(self) -> List[float]:
        return CircuitV._y_node_varray(self)

    @property
    def name(self) -> str:
        return CircuitS._name(self)

    @property
    def disable(self) -> str:
        return CircuitS._disable(self)

    @property
    def enable(self) -> str:
        return CircuitS._enable(self)

    @property
    def num_ckt_elements(self) -> int:
        return CircuitI._num_ckt_elements(self)

    @property
    def num_buses(self) -> int:
        return CircuitI._num_buses(self)

    @property
    def num_nodes(self) -> int:
        return CircuitI._num_nodes(self)

    @property
    def pc_element_first(self) -> int:
        return CircuitI._pc_element_first(self)

    @property
    def pc_element_next(self) -> int:
        return CircuitI._pc_element_next(self)

    @property
    def pd_element_first(self) -> int:
        return CircuitI._pd_element_first(self)

    @property
    def pd_element_next(self) -> int:
        return CircuitI._pd_element_next(self)

    @property
    def sample(self) -> int:
        return CircuitI._sample(self)

    @property
    def save_sample(self) -> int:
        return CircuitI._save_sample(self)

    @property
    def first_element(self) -> int:
        return CircuitI._first_element(self)

    @property
    def next_element(self) -> int:
        return CircuitI._next_element(self)

    @property
    def update_storage_t(self):
        return CircuitI._update_storage_t(self)

    @property
    def parent_pd_element(self) -> int:
        return CircuitI._parent_pd_element(self)

    @property
    def end_of_time_step_update(self) -> int:
        return CircuitI._end_of_time_step_update(self)

    def activate_bus(self, active_bus: str):
        return CircuitS._activate_bus(self, active_bus)

    def activate_bus_i_on_circuit(self, i: int) -> int:
        return CircuitI._activate_bus_i(self, i)

    def activate_element_on_circuit(self, active_element: str):
        return CircuitS._activate_element(self, active_element)

    def activate_class_on_circuit(self, argument: str) -> str:
        return CircuitS._activate_class(self, argument)
