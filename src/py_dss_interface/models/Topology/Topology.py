# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Topology.TopologyI import TopologyI
from py_dss_interface.models.Topology.TopologyS import TopologyS
from py_dss_interface.models.Topology.TopologyV import TopologyV


class Topology(TopologyI, TopologyV, TopologyS):
    """
    This interface implements the Topology (ITopology) interface of OpenDSS by declaring 3 procedures for accessing
    the different properties included in this interface: TopologyI, TopologyV, TopologyS.
    """
    pass
