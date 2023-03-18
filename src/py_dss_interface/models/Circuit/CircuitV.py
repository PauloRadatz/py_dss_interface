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

    def _losses(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 0)

    def _line_losses(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 1)

    def _substation_losses(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 2)

    def _total_power(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 3)

    def _buses_volts(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 4)

    def _buses_vmag(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 5)

    def _elements_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 6)

    def _buses_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 7)

    def _elements_losses(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 8)

    def _buses_vmag_pu(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 9)

    def _nodes_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 10)

    def _system_y(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 11)

    def _buses_distances(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 12)

    def _nodes_distances(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 13)

    def _nodes_vmag_by_phase(self, argument: int = 1) -> List[float]:
        argument = Base._check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 14, argument)

    def _nodes_vmag_pu_by_phase(self, argument: int = 1) -> List[float]:
        argument = Base._check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 15, argument)

    def _nodes_distances_by_phase(self, argument: int = 1) -> List[float]:
        argument = Base._check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 16, argument)

    def _nodes_names_by_phase(self, argument: int = 1) -> List[str]:
        argument = Base._check_int_param(argument, default=1)  # Phase 1 as default
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 17, argument)

    def _y_node_varray(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 18)

    def _y_node_order(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 19)

    def _y_currents(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CircuitV, 20)
