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

    def _num_nodes(self) -> int:
        return self._dss_obj.BUSI(0, 0)

    def _zsc_refresh(self) -> int:
        result = self._dss_obj.BUSI(1, 0) # TODO
        return result

    def _coord_defined(self) -> int:
        result = self._dss_obj.BUSI(2, 0)
        return result
    def _unique_node_number(self, start_number: int) -> int:  # TODO
        return self._dss_obj.BUSI(3, start_number)

    def _total_customers(self) -> int:
        return self._dss_obj.BUSI(4, 0)

    def _section_id(self) -> int:
        return self._dss_obj.BUSI(5, 0)
