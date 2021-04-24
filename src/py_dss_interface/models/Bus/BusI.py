# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class BusI(Base):
    def bus_integer(self, first, second):
        if isinstance(first, int) and isinstance(second, int) and first > 0:
            return int(self.dss_obj.BUSI(ctypes.c_int32(first), ctypes.c_int32(second)))
        else:
            return -1

    # BusI (int)
    def bus_num_nodes(self):
        """Returns the number of nodes of this bus."""
        return self.bus_integer(0, 0)

    def bus_zsc_refresh(self):
        """Recomputes Zsc for active bus for present circuit configuration.
        Return 1 if the procedure was successful."""
        return self.bus_integer(1, 0)

    def bus_coord_defined(self):
        """Returns 1 if a coordinate has been defined for this bus; otherwise, it will return 0."""
        return self.bus_integer(2, 0)

    def bus_get_unique_node_number(self):
        """Returns a unique node number at the active bus to avoid node collisions and adds it to the node list for
        the bus. The start number can be specified in the argument. """
        return self.bus_integer(3, 0)

    def bus_total_customers(self):
        """Returns returns the total number of customers served down line from this bus."""
        return self.bus_integer(4, 0)

    def bus_section_id(self):
        """Returns the integer ID of the feeder section in which this bus is located."""
        return self.bus_integer(5, 0)
