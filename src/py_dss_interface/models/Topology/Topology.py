# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Topology.TopologyI import TopologyI
from py_dss_interface.models.Topology.TopologyS import TopologyS
from py_dss_interface.models.Topology.TopologyV import TopologyV
from typing import List


class Topology(TopologyI, TopologyV, TopologyS):
    """
    This interface implements the Topology (ITopology) interface of OpenDSS by declaring 3 procedures for accessing
    the different properties included in this interface: TopologyI, TopologyV, TopologyS.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def num_loops(self) -> int:
        return TopologyI._num_loops(self)

    @property
    def num_isolated_branches(self) -> int:
        return TopologyI._num_isolated_branches(self)

    @property
    def num_isolated_loads(self) -> int:
        return TopologyI._num_isolated_loads(self)

    def first(self) -> int:
        return TopologyI._first(self)

    def next(self) -> int:
        return TopologyI._next(self)

    @property
    def active_branch(self) -> int:
        return TopologyI._active_branch(self)

    def forward_branch(self) -> int:
        return TopologyI._forward_branch(self)

    def backward_branch(self) -> int:
        return TopologyI._backward_branch(self)

    def looped_branch(self) -> int:
        return TopologyI._looped_branch(self)

    def parallel_branch(self) -> int:
        return TopologyI._parallel_branch(self)

    def first_load(self) -> int:
        return TopologyI._first_load(self)

    def next_load(self) -> int:
        return TopologyI._next_load(self)

    @property
    def active_level(self) -> int:
        return TopologyI._active_level(self)

    @property
    def branch_name(self) -> str:
        return TopologyS._branch_name_read(self)

    @branch_name.setter
    def branch_name(self, argument: str):
        TopologyS._branch_name_write(self, argument)

    @property
    def bus_name(self) -> str:
        return TopologyS._bus_name_read(self)

    @bus_name.setter
    def bus_name(self, argument: str):
        TopologyS._bus_name_write(self, argument)

    @property
    def all_looped_pairs(self) -> List[str]:
        return TopologyV._all_looped_pairs(self)

    @property
    def all_isolated_branches(self) -> List[str]:
        return TopologyV._all_isolated_branches(self)

    @property
    def all_isolated_loads(self) -> List[str]:
        return TopologyV._all_isolated_loads(self)
