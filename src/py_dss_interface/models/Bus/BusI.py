# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Base import Base


class BusI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t BUSI(int32_t Parameter, int32_t Argument)

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def bus_num_nodes(self) -> int:
        """Returns the number of nodes of this bus."""
        return self.dss_obj.BUSI(0, 0)

    def bus_zsc_refresh(self) -> int:
        """Recomputes Zsc for active bus for present circuit configuration. Return 1 if the procedure was successful."""
        result = self.dss_obj.BUSI(1, 0)
        Base.check_assertion_result(result, "Zsc recomputes failed!", "Zsc can not be recomputed!", expected_value=1)
        return result

    def bus_coord_defined(self) -> int:
        """Returns 1 if a coordinate has been defined for this bus; otherwise, it will return 0."""
        result = self.dss_obj.BUSI(2, 0)
        Base.check_assertion_result(result, "Bus coordinate not defined!", "Bus coordinates not find! The program "
                                                                           "will run normally",
                                    expected_value=1)
        return result

    def bus_get_unique_node_number(self, start_number: int = 1) -> int:
        """Returns a unique node number at the active bus to avoid node collisions and adds it to the node list for
        the bus. The start number can be specified in the second parameter.

        :param start_number: The first number corresponding the initial bus node number
        :returns: int
        :rtype: int
        """
        start_number = Base.check_int_param(start_number)
        return self.dss_obj.BUSI(3, start_number)

    def bus_total_customers(self) -> int:
        """Returns returns the total number of customers served down line from this bus."""
        return self.dss_obj.BUSI(4, 0)

    def bus_section_id(self) -> int:
        """Returns the integer ID of the feeder section in which this bus is located."""
        return self.dss_obj.BUSI(5, 0)
