# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class TopologyI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t TopologyI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def topology_num_loops(self) -> int:
        """Gets the number of loops."""
        return self.dss_obj.TopologyI(ctypes.c_int32(0), ctypes.c_int32(0))

    def topology_num_isolated_branches(self) -> int:
        """Gets the number of isolated branches (PD elements and capacitors)."""
        return self.dss_obj.TopologyI(ctypes.c_int32(1), ctypes.c_int32(0))

    def topology_num_isolated_loadss(self) -> int:
        """Gets the number of isolated loads."""
        return self.dss_obj.TopologyI(ctypes.c_int32(2), ctypes.c_int32(0))

    def topology_first(self) -> int:
        """Sets the first branch active, returns 0 if none."""
        return self.dss_obj.TopologyI(ctypes.c_int32(3), ctypes.c_int32(0))

    def topology_next(self) -> int:
        """Sets the next branch active, returns 0 if none."""
        return self.dss_obj.TopologyI(ctypes.c_int32(4), ctypes.c_int32(0))

    def topology_active_branch(self) -> int:
        """Returns the index of the active Branch."""
        return self.dss_obj.TopologyI(ctypes.c_int32(5), ctypes.c_int32(0))

    def topology_forward_branch(self) -> int:
        """Moves forward in the tree, return index of new active branch or 0 if no more."""
        return self.dss_obj.TopologyI(ctypes.c_int32(6), ctypes.c_int32(0))

    def topology_backward_branch(self) -> int:
        """Moves back toward the source, return index of new active branch or 0 if no more."""
        return self.dss_obj.TopologyI(ctypes.c_int32(7), ctypes.c_int32(0))

    def topology_looped_branch(self) -> int:
        """Moves to looped branch, return index or 0 if none."""
        return self.dss_obj.TopologyI(ctypes.c_int32(8), ctypes.c_int32(0))

    def topology_parallel_branch(self) -> int:
        """Mode to directly parallel branch, return index or 0 if none."""
        return self.dss_obj.TopologyI(ctypes.c_int32(9), ctypes.c_int32(0))

    def topology_first_load(self) -> int:
        """Sets as active load the first load at the active branch, return index or 0 if none."""
        return self.dss_obj.TopologyI(ctypes.c_int32(10), ctypes.c_int32(0))

    def topology_next_load(self) -> int:
        """Sets as active load the next load at the active branch, return index or 0 if none."""
        return self.dss_obj.TopologyI(ctypes.c_int32(11), ctypes.c_int32(0))

    def topology_active_level(self) -> int:
        """Gets the topological depth of the active branch."""
        return self.dss_obj.TopologyI(ctypes.c_int32(12), ctypes.c_int32(0))
